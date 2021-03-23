import re

line = "1	Cufflinks	exon	3631	3913	.	+	.	gene_id \"XLOC_000001\"; transcript_id \"TCONS_00000001\"; exon_number \"1\"; gene_name \"NAC001\"; oId \"transcript:AT1G01010.1\"; nearest_ref \"transcript:AT1G01010.1\"; class_code \"=\"; tss_id \"TSS1\"; p_id \"P1\";"
cuffID = re.findall(r'gene_id \"([\w\.]+)"', line) ##use RE to get lists of cuffid, ensemblId etc
cuffTx = re.findall(r'transcript_id \"([\w\.]+)"', line)
ensemblTx = re.findall(r'oId \"transcript:([\w\.]+)"', line)
geneName = re.findall(r'gene_name \"([\w\.]+)"', line)
geneId = re.findall(r'nearest_ref \"transcript:([\w\.]+)"', line)

print(ensemblTx)