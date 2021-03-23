from PIL import Image
import os
from tqdm import tqdm

def changeImage(file, inputdir, outputdir, currentdir):
    
    im = Image.open(os.path.join(inputdir, currentdir, file))
    nx, ny = im.size
    im2 = im.resize((int(nx/2.5), int(ny/2.5)), Image.BICUBIC)
    im2.save(os.path.join(outputdir, currentdir, file) ,dpi=(46,46))

if __name__ == '__main__':
    inputFolder = '../simple_CycleGAN/checkpoints/cycleGan_demo_flir/test_results/'
    outputFolder = 'output'
    dirlist = next(os.walk(inputFolder))[1]
    for dr in dirlist:
        if not os.path.exists(dr):
            os.mkdir(os.path.join(outputFolder,dr))
        for file in tqdm(os.listdir(os.path.join(inputFolder,dr))):    #get list of files
            changeImage(file, inputFolder, outputFolder, dr)