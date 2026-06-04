import os
import re
import time
import shutil
import threading
from datetime import datetime

import numpy as np
import soundfile as sf
import pyaudiowpatch as pyaudio


CHUNK = 1024
CHUNK_MINUTES = 10
TEMP_DIR = "_meeting_audio_chunks"
LOG_FILE = "meeting_helper_log.txt"

INCLUDE_FULL_TRANSCRIPT_IN_REPORT = False

# Better transcription quality. Use "small" or "medium" if your computer can handle it.
WHISPER_MODEL_SIZE = "small"


def log(msg):
    with open(LOG_FILE, "a", encoding="utf-8") as f:
        f.write(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] {msg}\n")


class TeamsMeetingCLI:
    def __init__(self):
        self.recording = False
        self.notes = []
        self.chunk_files = []

    def list_devices(self):
        p = pyaudio.PyAudio()
        devices = []

        print("\nAvailable WASAPI loopback devices:\n")

        for i in range(p.get_device_count()):
            info = p.get_device_info_by_index(i)

            if info.get("isLoopbackDevice", False):
                devices.append(info)
                print(
                    f"{len(devices)-1}: {info['name']} | "
                    f"channels={info['maxInputChannels']} | "
                    f"rate={int(info['defaultSampleRate'])}"
                )

        p.terminate()
        return devices

    def select_device(self, devices):
        while True:
            choice = input("\nSelect Teams loopback device number: ").strip()

            if choice.isdigit():
                index = int(choice)
                if 0 <= index < len(devices):
                    return devices[index]

            print("Invalid selection.")

    def prepare_temp_dir(self):
        if os.path.exists(TEMP_DIR):
            shutil.rmtree(TEMP_DIR)

        os.makedirs(TEMP_DIR, exist_ok=True)

    def record_audio_in_chunks(self, device_info):
        p = pyaudio.PyAudio()

        try:
            channels = int(device_info["maxInputChannels"])
            rate = int(device_info["defaultSampleRate"])

            stream = p.open(
                format=pyaudio.paInt16,
                channels=channels,
                rate=rate,
                input=True,
                input_device_index=int(device_info["index"]),
                frames_per_buffer=CHUNK,
            )

            chunk_seconds = CHUNK_MINUTES * 60
            max_frames_per_chunk = rate * chunk_seconds

            chunk_number = 1
            frames_written = 0

            current_file_path = os.path.join(
                TEMP_DIR,
                f"chunk_{chunk_number:04d}.wav"
            )

            audio_file = sf.SoundFile(
                current_file_path,
                mode="w",
                samplerate=rate,
                channels=channels,
                subtype="PCM_16"
            )

            self.chunk_files.append(current_file_path)
            log(f"Started chunk: {current_file_path}")

            print("\nRecording started.")
            print(f"Audio is saved temporarily in {CHUNK_MINUTES}-minute chunks.")
            print("Type notes anytime.")
            print("Type /stop to finish.\n")

            while self.recording:
                data = stream.read(CHUNK, exception_on_overflow=False)

                audio_array = np.frombuffer(data, dtype=np.int16)
                audio_array = audio_array.reshape(-1, channels)

                audio_file.write(audio_array)
                frames_written += len(audio_array)

                if frames_written >= max_frames_per_chunk:
                    audio_file.close()

                    chunk_number += 1
                    frames_written = 0

                    current_file_path = os.path.join(
                        TEMP_DIR,
                        f"chunk_{chunk_number:04d}.wav"
                    )

                    audio_file = sf.SoundFile(
                        current_file_path,
                        mode="w",
                        samplerate=rate,
                        channels=channels,
                        subtype="PCM_16"
                    )

                    self.chunk_files.append(current_file_path)
                    log(f"Started chunk: {current_file_path}")

            audio_file.close()
            stream.stop_stream()
            stream.close()

            log("Recording stopped")

        finally:
            p.terminate()

    def note_input(self):
        while self.recording:
            text = input("> ").strip()

            if text.lower() == "/stop":
                self.recording = False
                break

            if text:
                timestamp = datetime.now().strftime("%H:%M:%S")
                self.notes.append(f"[{timestamp}] {text}")
                log(f"Note added: {text}")

    def transcribe_chunks(self):
        from faster_whisper import WhisperModel

        print("\nLoading transcription model...")
        model = WhisperModel(WHISPER_MODEL_SIZE, device="cpu", compute_type="int8")

        transcript_parts = []

        total_chunks = len(self.chunk_files)

        for index, chunk_file in enumerate(self.chunk_files, 1):
            if not os.path.exists(chunk_file):
                continue

            print(f"\nTranscribing chunk {index}/{total_chunks}: {chunk_file}")
            log(f"Transcribing {chunk_file}")

            segments, _ = model.transcribe(
                chunk_file,
                beam_size=5,
                vad_filter=True,
                language="en",
                task="transcribe",
                condition_on_previous_text=True
            )

            for segment in segments:
                text = segment.text.strip()
                if text:
                    transcript_parts.append(text)

            try:
                os.remove(chunk_file)
                log(f"Deleted chunk: {chunk_file}")
            except Exception as e:
                log(f"Could not delete chunk {chunk_file}: {e}")

        del model

        return " ".join(transcript_parts)

    def split_sentences(self, text):
        """Split transcript into usable sentence-like chunks."""
        chunks = re.split(r'(?<=[.!?])\s+', text)
        return [c.strip() for c in chunks if len(c.strip()) > 25]

    def normalise_transcript_text(self, text):
        """Fix common speech-to-text mistakes before extracting meaning.

        These replacements are intentionally conservative and meeting-domain specific.
        Add your own phrases here as you notice repeated Whisper mistakes.
        """
        replacements = {
            r"\bS\s*a\s*pay\b": "SAP",
            r"\bS\s*A\s*P\b": "SAP",
            r"\bI\s*am\b": "Internal Moderator",
            r"\bIM\b": "Internal Moderator",
            r"\bblack board\b": "Blackboard",
            r"\bshare point\b": "SharePoint",
            r"\bCRNs?\b": "CRN",
            r"\bKHA\b": "KHA",
            r"\bBelfast\b": "Belfast",
            r"\bgood to go with you\b": "good to go",
            r"\bgreenlight\b": "green light",
            r"\breflashing\b": "refreshing",
            r"\breflash\b": "refresh",
        }
        cleaned = text
        for pattern, repl in replacements.items():
            cleaned = re.sub(pattern, repl, cleaned, flags=re.I)
        cleaned = re.sub(r'\s+', ' ', cleaned).strip()
        return cleaned

    def clean_sentence(self, sentence):
        """Light cleanup for repeated filler words and spacing."""
        sentence = self.normalise_transcript_text(sentence)
        sentence = re.sub(r'\b(um|uh|erm|yeah|you know|kind of|sort of)\b[, ]*', '', sentence, flags=re.I)
        return sentence.strip()

    def rank_sentences(self, sentences, keywords, limit=12):
        """Score sentences by meeting-specific keywords instead of taking the first N."""
        ranked = []
        for i, sentence in enumerate(sentences):
            clean = self.clean_sentence(sentence)
            lower = clean.lower()
            score = 0
            for keyword, weight in keywords.items():
                if keyword in lower:
                    score += weight
            # Prefer informative sentences, but avoid extremely long transcript rambling.
            if 60 <= len(clean) <= 450:
                score += 1
            if score > 0:
                ranked.append((score, i, clean))

        ranked.sort(key=lambda x: (-x[0], x[1]))

        results = []
        seen = set()
        for _, _, sentence in ranked:
            key = sentence[:90].lower()
            if key not in seen:
                results.append(sentence)
                seen.add(key)
            if len(results) >= limit:
                break
        return results

    def extract_summary(self, text):
        sentences = self.split_sentences(text)
        keywords = {
            "sap": 5,
            "moderation": 5,
            "coursework": 4,
            "module": 3,
            "folder": 4,
            "blackboard": 4,
            "students": 3,
            "internal moderator": 5,
            "deadline": 4,
            "green light": 5,
            "good to go": 5,
            "sharepoint": 3,
            "version control": 4,
            "staff": 3,
            "preparation": 3,
            "release": 3,
            "email": 2,
        }
        return self.rank_sentences(sentences, keywords, limit=15)

    def extract_system_explanation(self, text):
        sentences = self.split_sentences(text)
        keywords = {
            "folder": 5,
            "folders": 5,
            "home for each module": 6,
            "module": 3,
            "crn": 4,
            "coursework": 4,
            "moderation": 5,
            "internal moderator": 5,
            "blackboard": 5,
            "staff": 4,
            "preparation stage": 6,
            "sharepoint": 4,
            "version control": 4,
            "comments": 3,
            "status": 3,
            "stage one": 4,
            "stage two": 4,
            "stage three": 4,
        }
        return self.rank_sentences(sentences, keywords, limit=12)

    def extract_actions(self, text):
        sentences = self.split_sentences(text)
        keywords = {
            "need to": 5,
            "needs to": 5,
            "you need": 5,
            "i will": 4,
            "we will": 4,
            "action point": 5,
            "follow up": 4,
            "send": 4,
            "submit": 4,
            "email": 4,
            "prepare": 4,
            "complete": 4,
            "update": 3,
            "check": 3,
            "deadline": 5,
            "before": 3,
            "by the": 4,
            "green light": 6,
            "good to go": 6,
            "please look out": 5,
            "blackboard": 4,
            "internal moderator": 5,
        }
        return self.rank_sentences(sentences, keywords, limit=25)

    def infer_next_steps(self, text):
        """Create practical instructions from recurring meeting themes.
        This is still rule-based, but much more useful than raw sentence extraction.
        """
        lower = text.lower()
        steps = []

        if "green light" in lower or "good to go" in lower:
            steps.append("Wait for Peter's green-light / good-to-go email before using the new folders, because the structure may still be refreshed.")
        if "deadline tomorrow" in lower and "email" in lower:
            steps.append("If something is due immediately, send it to the Internal Moderator by email first, then upload/track it in the system once Peter confirms it is ready.")
        if "folder" in lower and "module" in lower:
            steps.append("Use the folder for the correct module or module instance; where there are Belfast/KHA or duplicate instances, make sure the right folder is populated.")
        if "coursework" in lower or "moderation" in lower:
            steps.append("Put the coursework and moderation materials into the relevant module folder and complete the moderation form/details there.")
        if "blackboard" in lower:
            steps.append("Use Blackboard for student-facing release/submission unless your school gives a different process; the new system is mainly for staff preparation and moderation.")
        if "version control" in lower or "sharepoint" in lower:
            steps.append("For versions/comments, rely on SharePoint version history and the comments/form narrative unless programme leadership gives a stricter naming process.")
        if "15th" in lower or "19th" in lower:
            steps.append("Check the agreed dates: the discussion mentioned moderation completion around the 15th and student release around the 19th, but Peter may confirm the final dates by email.")

        return steps

    def detected(self, text, *phrases):
        lower = text.lower()
        return any(p.lower() in lower for p in phrases)

    def build_interpreted_system_explanation(self, text):
        """Create a plain-English explanation of the system from detected meeting themes."""
        text = self.normalise_transcript_text(text)
        points = []

        if self.detected(text, "staff", "preparation stage", "creating the material"):
            points.append("The new area is mainly for staff preparation and moderation of coursework materials, not for student submission.")
        if self.detected(text, "folder", "folders", "home for each module"):
            points.append("Each module or module instance has a dedicated folder that acts as the home for its coursework and moderation material.")
        if self.detected(text, "crn", "belfast", "kha", "multiple"):
            points.append("Where a module has multiple CRNs or instances, such as Belfast and KHA, use the correct folder for each instance and clearly flag/copy shared material where needed.")
        if self.detected(text, "refresh", "green light", "good to go", "dates"):
            points.append("Peter still needed to refresh the table/folders and update dates, so the system should not be used until he confirms it is ready.")
        if self.detected(text, "coursework", "moderation", "internal moderator"):
            points.append("The workflow is to prepare coursework, place the moderation material in the correct folder, and send it through internal moderation.")
        if self.detected(text, "sharepoint", "version control", "comments", "conversation"):
            points.append("SharePoint version history and comments can support version control, but the meeting did not define a strict final naming/version process.")
        if self.detected(text, "blackboard", "students"):
            points.append("Blackboard remains the student-facing place for releasing work or handling student submission, unless programme leadership confirms otherwise.")

        return points

    def build_interpreted_key_points(self, text):
        text = self.normalise_transcript_text(text)
        points = []

        if self.detected(text, "sap", "coursework"):
            points.append("SAP coursework needs to go through the same preparation and moderation discipline as normal module coursework.")
        if self.detected(text, "deadline tomorrow", "email initially"):
            points.append("For urgent deadlines, the immediate workaround is to email the Internal Moderator first and update the system later.")
        if self.detected(text, "green light", "good to go"):
            points.append("Peter will confirm when the folder structure is ready to use; until then, avoid uploading into the new folders.")
        if self.detected(text, "15th", "19th"):
            points.append("Two important dates were discussed: moderation completion around the 15th and student release/finalisation around the 19th, subject to Peter's confirmation.")
        if self.detected(text, "blackboard"):
            points.append("There was uncertainty about exactly what needs changing in Blackboard, so Blackboard changes should be confirmed before students are notified.")
        if self.detected(text, "year 2", "d7", "brs"):
            points.append("There were curriculum/admin updates mentioned, including no D7 in Year 2 and updated BRS documents in the preliminary SharePoint folder.")
        if self.detected(text, "rubrics", "outcomes", "uk standards", "benchmarks"):
            points.append("The group also discussed improving assessment design, rubrics and outcomes so coursework can properly discriminate between grade bands and align with standards.")

        return points

    def merge_interpreted_with_raw(self, interpreted, raw, limit):
        """Prefer clean interpreted points, then fill any gaps with high-ranked raw sentences."""
        results = []
        seen = set()
        for item in interpreted + raw:
            key = item[:90].lower()
            if key not in seen:
                results.append(item)
                seen.add(key)
            if len(results) >= limit:
                break
        return results

    def create_report(self, title, summary, system_explanation, next_steps, actions, notes, transcript):
        report = f"""MEETING SUMMARY

Meeting: {title}
Date: {datetime.now().strftime('%Y-%m-%d %H:%M')}

========================
KEY DISCUSSION POINTS
========================
"""

        if summary:
            for i, item in enumerate(summary, 1):
                report += f"{i}. {item}\n"
        else:
            report += "No clear discussion points detected.\n"

        report += """

========================
SYSTEM EXPLANATION
========================
"""

        if system_explanation:
            for i, item in enumerate(system_explanation, 1):
                report += f"{i}. {item}\n"
        else:
            report += "No clear system explanation detected.\n"

        report += """

========================
WHAT YOU SHOULD DO NEXT
========================
"""

        if next_steps:
            for i, item in enumerate(next_steps, 1):
                report += f"{i}. {item}\n"
        else:
            report += "No clear next steps inferred.\n"

        report += """

========================
RAW ACTION-LIKE SENTENCES
========================
"""

        if actions:
            for i, item in enumerate(actions, 1):
                report += f"{i}. {item}\n"
        else:
            report += "No clear action points detected.\n"

        report += """

========================
MANUAL NOTES
========================
"""

        if notes:
            report += "\n".join(notes)
        else:
            report += "No manual notes entered."

        if INCLUDE_FULL_TRANSCRIPT_IN_REPORT:
            report += """

========================
FULL TRANSCRIPT
========================
"""
            report += transcript

        return report

    def cleanup_temp_dir(self):
        if os.path.exists(TEMP_DIR):
            try:
                shutil.rmtree(TEMP_DIR)
                log("Temporary chunk folder deleted")
            except Exception as e:
                log(f"Could not delete temp folder: {e}")

    def read_existing_transcript(self, transcript_path):
        """Read a transcript file that already exists, instead of recording/transcribing."""
        transcript_path = transcript_path.strip().strip('"').strip("'")

        if not transcript_path:
            raise ValueError("Transcript path cannot be empty.")

        if not os.path.exists(transcript_path):
            raise FileNotFoundError(f"Transcript file not found: {transcript_path}")

        with open(transcript_path, "r", encoding="utf-8") as f:
            transcript = f.read().strip()

        if not transcript:
            raise ValueError("Transcript file is empty.")

        log(f"Loaded existing transcript: {transcript_path}")
        return transcript

    def analyse_transcript_and_save_report(self, title, transcript, notes=None, save_transcript_copy=True):
        """Run the summary/action extraction on transcript text and save outputs."""
        notes = notes or []
        combined_text = transcript + "\n" + "\n".join(notes)

        combined_text = self.normalise_transcript_text(combined_text)

        raw_summary = self.extract_summary(combined_text)
        raw_system_explanation = self.extract_system_explanation(combined_text)

        summary = self.merge_interpreted_with_raw(
            self.build_interpreted_key_points(combined_text),
            raw_summary,
            limit=12
        )
        system_explanation = self.merge_interpreted_with_raw(
            self.build_interpreted_system_explanation(combined_text),
            raw_system_explanation,
            limit=12
        )
        next_steps = self.infer_next_steps(combined_text)
        actions = self.extract_actions(combined_text)

        report = self.create_report(
            title,
            summary,
            system_explanation,
            next_steps,
            actions,
            notes,
            transcript
        )

        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        output_file = f"meeting_summary_{timestamp}.txt"

        with open(output_file, "w", encoding="utf-8") as f:
            f.write(report)

        transcript_file = None
        if save_transcript_copy:
            transcript_file = f"meeting_transcript_{timestamp}.txt"
            with open(transcript_file, "w", encoding="utf-8") as f:
                f.write(transcript)

        return output_file, transcript_file

    def run_from_existing_transcript(self):
        title = input("\nMeeting title: ").strip() or "Untitled Meeting"
        transcript_path = input("Path to existing transcript .txt file: ").strip()

        transcript = self.read_existing_transcript(transcript_path)

        output_file, _ = self.analyse_transcript_and_save_report(
            title=title,
            transcript=transcript,
            notes=[],
            save_transcript_copy=False
        )

        print("\nDone.")
        print(f"Summary/action points saved: {output_file}")

    def run(self):
        print("Teams Meeting Helper - Chunked CLI Version")
        print("------------------------------------------")

        print("\nChoose mode:")
        print("1. Record meeting audio, transcribe, then summarise")
        print("2. Use an existing transcript file, then summarise")

        mode = input("\nEnter 1 or 2: ").strip()

        if mode == "2":
            self.run_from_existing_transcript()
            return

        if mode != "1":
            print("Invalid mode. Exiting.")
            return

        permission = input(
            "\nDo you have permission to record/transcribe this meeting? yes/no: "
        ).strip().lower()

        if permission not in ["yes", "y"]:
            print("Permission is required. Exiting.")
            return

        title = input("\nMeeting title: ").strip() or "Untitled Meeting"

        devices = self.list_devices()

        if not devices:
            print("No WASAPI loopback devices found.")
            return

        selected_device = self.select_device(devices)

        print(f"\nSelected: {selected_device['name']}")

        self.prepare_temp_dir()

        self.recording = True

        record_thread = threading.Thread(
            target=self.record_audio_in_chunks,
            args=(selected_device,),
            daemon=False
        )

        notes_thread = threading.Thread(
            target=self.note_input,
            daemon=False
        )

        record_thread.start()
        notes_thread.start()

        notes_thread.join()

        self.recording = False

        if record_thread.is_alive():
            record_thread.join(timeout=10)

        print("\nRecording stopped.")
        print(f"Total chunks recorded: {len(self.chunk_files)}")

        transcript = self.transcribe_chunks()

        output_file, transcript_file = self.analyse_transcript_and_save_report(
            title=title,
            transcript=transcript,
            notes=self.notes,
            save_transcript_copy=True
        )

        self.cleanup_temp_dir()

        print("\nDone.")
        print(f"Summary/action points saved: {output_file}")
        print(f"Transcript saved separately: {transcript_file}")
        print("Temporary audio chunks deleted.")


if __name__ == "__main__":
    try:
        app = TeamsMeetingCLI()
        app.run()

    except KeyboardInterrupt:
        print("\nStopped by user.")

    except Exception as e:
        log(f"Fatal error: {e}")
        print(f"\nError occurred. Check {LOG_FILE}")