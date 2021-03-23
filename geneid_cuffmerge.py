
gtf_handle = "./merged.gtf"
fh = open(gtf_handle, "r")

import re
from tqdm import tqdm

trans_ids = {}

with open('merged2.gtf', 'w') as f:

	for line in tqdm(fh):
		line = line.strip('\n') ##strip the line to remove white spaces
		#print (line)
		cuffID = re.findall(r'gene_id \"([\w\.]+)"', line) ##use RE to get lists of cuffid, ensemblId etc
		cuffTx = re.findall(r'transcript_id \"([\w\.]+)"', line)
		ensemblTx = re.findall(r'oId \"([\w\.]+)"', line)
		geneName = re.findall(r'gene_name \"([\w\.]+)"', line)
		geneId = re.findall(r'nearest_ref \"([\w\.]+)"', line)
		
		if geneId:
		#print (geneId[0].split(".")[0])
			line = str(line).replace(cuffID[0], str(geneId[0].split(".")[0]))
		if ensemblTx:
			line = str(line).replace(cuffTx[0], ensemblTx[0]) ##unlist the transcript identifiers and replace cufflinksID with ensemblIDs
		
		f.write("%s\n" % str(line)) ##write file out to a .gtf file
		