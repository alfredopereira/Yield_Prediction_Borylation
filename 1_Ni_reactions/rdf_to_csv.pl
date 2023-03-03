#!/usr/bin/perl
use strict;
use warnings;

foreach my $filename ( <*.rdf> ) {

    open INFILE,$filename;
    print $filename."\n";
    
    my @auxName = split("[.]",$filename);
    
    my $fileYield = $auxName[0]."_Yield.csv";
    open(OUTFILE_YIELD,">$fileYield");
    print OUTFILE_YIELD "id,y1,y2,y3,y4,y5,y6,y7,y8,y9,y10";
    
    my $fileRGT = $auxName[0]."_RGT_cas.csv";
    open(OUTFILE_RGT,">$fileRGT");
    print OUTFILE_RGT "id,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10";
    
    my $fileCAT = $auxName[0]."_CAT_cas.csv";
    open(OUTFILE_CAT,">$fileCAT");
    print OUTFILE_CAT "id,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10";
    
    my $fileSOL = $auxName[0]."_SOL_cas.csv";
    open(OUTFILE_SOL,">$fileSOL");
    print OUTFILE_SOL "id,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10";
    
    my $fileInfo = $auxName[0]."_Info.csv";
    open(OUTFILE_INFO,">$fileInfo");
    print OUTFILE_INFO "id,scheme,schemeFile,nRCT,nPRO,stages";
    
    my $linea;
    my $temp = 0;
    my $temp2 = 0;
    
    my @scheme;
    my @nRePr;
    my @stages;
    
    my @yield;
    my @rgt;
    my @cat;
    my @sol;

    my $schemeCount = 0;
    my $count = 1;

        
    while ( $linea = <INFILE> ) {
        chomp($linea);
        
        if ($linea =~ /:STEPS/ ) {
            $temp = 0;
            $temp2 = 0;
            print OUTFILE_INFO "\n$count";
            print OUTFILE_YIELD "\n$count";
            print OUTFILE_RGT "\n$count";
            print OUTFILE_CAT "\n$count";
            print OUTFILE_SOL "\n$count";
            $count++;
        }        
        if ($linea =~ /RFMT/ ) {
            @scheme = split(" ",$linea);
            $temp = -1;
            $schemeCount++;
        }        
        if ($linea =~ /^  / && $temp == -1 ) {
            @nRePr = split("  ", $linea);
            $temp = 0; 
        }       
        if ($linea =~ /:STAGES/ ) {
            $temp = 1;
        }
        if ($linea =~ /:YIELD/ ) {
            $temp = 2;
        }
        if ($linea =~ /:RGT/ ) {
            $temp = 3; 
        }
        if ($linea =~ /:CAT/ ) {
            $temp = 4; 
        }
        if ($linea =~ /:SOL/ ) {
            $temp = 5; 
        }
        
        if ($linea =~ /DATUM/ && $temp == 1 && $temp2 == 0) {
            @stages = split(" ", $linea);
            print OUTFILE_INFO ",".$schemeCount.",".$scheme[2].",".$nRePr[1].",".$nRePr[2].",".$stages[1];
        }
        if ($linea =~ /DATUM/ && $temp == 2 && $temp2 == 0) {
            @yield = split(" ", $linea);
            print OUTFILE_YIELD ",".$yield[1]; 
        }
        if ($linea =~ /DATUM/ && $temp == 3 && $temp2 == 0) {
            @rgt = split(" ", $linea);
            print OUTFILE_RGT ",".$rgt[1]; 
        }
        if ($linea =~ /DATUM/ && $temp == 4 && $temp2 == 0) {
            @cat = split(" ", $linea);
            print OUTFILE_CAT ",".$cat[1]; 
        }
        if ($linea =~ /DATUM/ && $temp == 5 && $temp2 == 0) {
            @sol = split(" ", $linea);
            print OUTFILE_SOL ",".$sol[1]; 
        }
        
        if ($linea =~ /NOTES/ || $linea =~ /TITLE/ || $linea =~ /EXP_PROC/) {
            $temp2 = 1; 
        }
    }
    close INFILE;
}
