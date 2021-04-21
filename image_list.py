import os
import numpy as np
import csv
import numpy as np
from tqdm import tqdm


def create_image_list_with_labels(root_folder):
    images = []
    with open('dsta.txt', 'w') as file:
        #writer = csv.writer(file, delimiter = '\t')
        f = sorted(os.listdir(root_folder))
        for class_id, image_class in tqdm(enumerate(f)):
        	
        	for filename in os.listdir(os.path.join(root_folder,image_class)):
        		#print(os.path.abspath(os.path.join(root_folder,image_class,filename)))

                
                    
                    #writer.writerow([os.path.abspath(os.path.join(root_folder,image_class,filename)), class_id])
                    file.write(os.path.abspath(os.path.join(root_folder,image_class,filename)+' '+str(class_id)+'\n'))

root_folder="flir"
create_image_list_with_labels(root_folder)
