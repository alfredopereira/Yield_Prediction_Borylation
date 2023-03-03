cat raw/*.rdf > Ni_borylation.rdf
perl rdf_to_csv.pl
molconvert smiles Ni_borylation.rdf -o Ni_borylation.smiles

