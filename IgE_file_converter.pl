#   Author: Ioannis Pandis

# Script to read all lines in the specific IgE files, 
# store all the data belonging to the same subject (the default file structure
# represents one line per measurement, resulting in multiple line per subject)
# in a hash array and finally print out a csv file with one line per subject
#############################################################

#use warnings;

# Initiate files #


#-----------------------------------------------------
$output = '/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/May2014/processed/2014-05-16-adult_ige_consolidated_TEST.csv'; # Set output file
open (OUT, ">$output"); #|| die print "\n\Could not write to the output file.$!\n"; # Open the output file


#READ IN FIRST FILE AND CREATE HASH
#-----------------------------------------------------
$input_file = '/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Adult_Data/May2014/processed/2014-05-16-adult_ige.csv'; #First list
open (INPUT, $input_file) || die print "\nCould not open input list\n"; # Open the file


### Specific IgE reactants  

my %igecol = (                      # Define a hash (%igecol) which tells which specific IgE should go intowhich column
"mixed_trees" => 0,
"dermatophagoides_pteronyssinus" => 1,
"dog" => 2,
"mixed_grasses" => 3,
"other" => 4,
"cat" => 5,
"aspergillus" => 6,
"alternaria" => 7,
"dermatophagoides_farinae" => 8,
"ambrosia" => 9,
"artemisia" => 10,
"olive" => 11
);

print OUT "subjid,mixed_trees,dermatophagoides_pteronyssinus,dog,mixed_grasses,other,cat,aspergillus,alternaria,dermatophagoides_farinae,ambrosia,artemisia,olive,rast_status\n";
 
my %hash = (); #Initiate the %hash hash :-)

while (my $line=<INPUT>){
	if($line=~/^subj/) {next}; ## Test to make sure it is not reading the first line
  chomp $line; # Removes \n or \r or \s from end of the line. Alternative: Chop removes last element
  my @data = split (/\,/, $line); # Split file at comma
  if ($data[0] ne ""){ #Capture the name in the in the first column of the input file. In this case it is the subject ID. ne: text not equal to empty 

  if(!defined($hash{$data[0]})) {    #Create the array inside %hash for each new subject (all zeros)
	for($i=0; $i < 12; $i++){$hash{$data[0]}[$i] = "."}
}
  $ige = $data[3]; #Set names for columns
  $val = $data[5];
  $col = $igecol{$ige}; #Set column number using the %igecol hash
  if(!defined($col)){print STDERR "Unknown ige $ige\n";$ige = "other";$col=$igecol{$ige}} #Check you have a know column name (sanity check)
 
  $hash{$data[0]}[$col]=$val; # Enter the specific IgE values in the appropriate column
   
#  if($data[0]>= 0.35 and $data[0] ne '.') {$hash{$data[0]}[12] = "POSITIVE"} else {$hash{$data[0]}[12] = "NEGATIVE"} 
  
  #or $data[1]>= 0.35 or $data[2]>= 0.35 or $data[3]>= 0.35 or $data[4]>= 0.35 or $data[5]>= 0.35 or $data[6]>= 0.35 or $data[7]>= 0.35 or $data[8]>= 0.35 or $data[9]>= 0.35 or $data[10]>= 0.35 or $data[11] >= 0.35){$hash{$data[0]}[12] = "POSITIVE"} else {$hash{$data[0]}[12] = "NEGATIVE"}


      }
}

#foreach my $key ( keys %hash )  {
#	for($x=0; $x < 12; $x++) {
#		if($hash{$key[$x]}>=0.35 && $hash{$key[$x]}[12] ne "POSITIVE"){
#			$hash{$key[$x]}[12] = "POSITIVE";
#		}
#		else {$hash{$key[$x]}[12] = "NEGATIVE";}
#	} 
#}

	  
foreach my $key ( keys %hash )  {
    print OUT "$key,";                                    #Print out hash key
    foreach ( @{$hash{$key}} )  {
        print OUT "$_,";				#Print out hash content for key
    }
    print OUT"\n";
}	  



close (INPUT); # Close the input function

close (OUTPUT);

