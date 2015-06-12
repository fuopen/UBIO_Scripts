#   Author: Ioannis Pandis

# Script to read all lines in the specific IgE files, 
# store all the data belonging to the same subject (the default file structure
# represents one line per measurement, resulting in multiple line per subject)
# in a hash array and finally print out a csv file with one line per subject
#############################################################

#use warnings;

# Initiate files #


#-----------------------------------------------------
$output = '/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/New_Baseline_Data_to_be_included_Nov_2014_release/processed/2014-05-16-pead_skin_prick_consolidated.csv'; # Set output file
open (OUT, ">$output"); #|| die print "\n\Could not write to the output file.$!\n"; # Open the output file


#READ IN FIRST FILE AND CREATE HASH
#-----------------------------------------------------
$input_file = '/Users/ip304/Documents/UBIOPRED/UBIOPRED_data/Paediatric_Data/New_Baseline_Data_to_be_included_Nov_2014_release/raw/2014-05-16-pead_skin_prick.csv'; #First list
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
);

@column_heads = ("mixed_trees", "dermatophagoides_pteronyssinus", "dog", "mixed_grasses", "other", "cat", "aspergillus", "alternaria", "dermatophagoides_farinae", "ambrosia", "artemisia");
print OUT "subjid";
foreach $head(@column_heads){print OUT ",$head\_wheal,$head\_flare"};
print OUT "\n";
 
my %hash = (); #Initiate the %hash hash :-)


while (my $line=<INPUT>){
   if($line=~/^subj/) {next}; ## Test to make sure it is not reading the first line
   chomp $line; # Removes \n or \r or \s from end of the line. Alternative: Chop removes last element
  my @data = split (/\,/, $line); # Split file at comma
  if ($data[0] ne ""){ #Capture the name in the in the first column of the input file. In this case it is the subject ID. ne: text not equal to empty 
  
  if(!defined($hash{$data[0]})) {    #Create the array inside %hash for each new subject (all zeros)
	for($i=0; $i < 22; $i++){$hash{$data[0]}[$i] = "."}
}
  $ige = $data[3]; #Set names for columns
  $val_wheal = $data[5];
  $val_flare = $data[6];
  $col = 2*$igecol{$ige}; #Set column number using the %igecol hash
  if(!defined($col)){print STDERR "Unknown ige $ige\n";$ige = "other";$col=$igecol{$ige}} #Check you have a know column name (sanity check)
 
  $hash{$data[0]}[$col]=$val_wheal; # Enter the specific IgE values in the appropriate column
  $col++;
  $hash{$data[0]}[$col]=$val_flare; # Enter the specific IgE values in the appropriate column

   
      


      }
}
	  
foreach my $key ( keys %hash )  {
    print OUT "$key,";                                    #Print out hash key
    foreach ( @{$hash{$key}} )  {
        print OUT "$_,";				#Print out hash content for key
    }
    print OUT"\n";
}	  



close (INPUT); # Close the input function

