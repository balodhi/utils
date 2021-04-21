import os
import numpy as np
from skimage.io import imread, imsave
from skimage.color import rgb2lab, lab2rgb
from skimage.util import img_as_ubyte
import numpy as np
from skimage.transform import resize

def load_images_from_folder(root_folder,target_folder):
    images = []
    for image_class in os.listdir(root_folder):
    	os.mkdir(os.path.join(target_folder,image_class))
    	for filename in os.listdir(os.path.join(root_folder,image_class)):
    		img = imread(os.path.join(root_folder,image_class,filename))
    		print(os.path.join(root_folder,image_class,filename))
    		if img.shape[-1]>3:
    			img = img[...,0:3]	 #remove the last alpha channel
    		im1 = rgb2lab(img)
    		im1[...,0] = im1[...,0] - 20
    		im1 = lab2rgb(im1)
    		im1 = resize(im1, (96, 96))
    		im1 = img_as_ubyte(im1)
    		imsave(os.path.join(target_folder,image_class,filename), im1)

root_folder="flir"
target_folder='target_flir'
load_images_from_folder(root_folder,target_folder)