cat raw/*.rdf > Co_borylation.rdf
perl rdf_to_csv.pl
molconvert smiles Co_borylation.rdf -o Co_borylation.smiles

