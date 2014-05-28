#   Author: Ioannis Pandis

# 
#############################################################
$path = "C:/Users/Kai/Desktop/";

#use warnings;
# Initiate files 

#-----------------------------------------------------
$output = $path."/adult_current_regular_meds_loading_fields_consolidated.csv"; # Set output file
open (OUT, ">$output") 
	|| die print "\n\Could not write to the output file.$!\n"; # Open the output file

#READ IN FIRST FILE AND CREATE HASH
#-----------------------------------------------------
$input_file = $path."/adult_current_regular_meds_loading_fields.csv"; #First list
open (INPUT, $input_file) 
	|| die print "\nCould not open input list\n"; # Open the file

print OUT "subjid,oral steroids,total daily dose OCS normalised to pred,ICS,ICS dose,ICS/LABA,LABA,injectable_steroids,SABA,mucolitic,antihistamine,antibiotic,macrolide,Nasal Steroids,antimuscarinic,xolair,anticholinergic,immunossuppressant,leukotriene modifier,mast cell stabiliser,SABA,topical steroids,antifungal,adreneline,anti cough,oxygen,PDE4 inh,saline,xanthines,LAMA,paraffin,NSAID,Immunotherapy\n";

#Initiate the %hash hash :-)
my %hash = (); 
while (my $line=<INPUT>){
	# Test to make sure it is not reading the first line
	if($line=~/^subj/) {next}; 
		
	# Removes \n or \r or \s from end of the line. Alternative: Chop removes last element
	chomp $line; 
	
	# Split file at comma
	my @data = split (/,/, $line); 

#print OUT "$data[0]\t$	data[3]\n";

	if ($data[0] ne ""){ 
	#Capture the name in the in the first column of the input file. In this case it is the subject ID. ne: text not equal to empty 

		if(!defined($hash{$data[0]})) {    
		#Create the array inside %hash for each new subject (all zeros)
			$hash{$data[0]}[0] = "no";
			$hash{$data[0]}[1] = ".";
			$hash{$data[0]}[2] = "no";
			$hash{$data[0]}[3] = ".";
			
			for($i=4; $i < 32; $i++){
				$hash{$data[0]}[$i] = "no"
			}
		}
   
		if($data[1] eq "yes"){
			$hash{$data[0]}[0]="yes";
#			print OUT "yes\n";
			}
		#else {next};
		
		if($data[2] ne ""){
			$hash{$data[0]}[1]=$data[2];
#			print OUT "$data[2]\n";
			}
		#else {next};
		
		if($data[3] eq "yes"){
			$hash{$data[0]}[2]="yes";
		#	print OUT "$data[0]\t$data[3]\n";
			}
		#else {next};
		
		if($data[4] ne ""){
			$hash{$data[0]}[3]=$data[4];
#			print "$data[2]\n";
			}
		#else {next};
		
		for ($j=4; $j < 32; $j++){
			if($data[$j+1] eq "yes"){
				$hash{$data[0]}[$j]="yes";
#			print OUT "yes\n";
			}
	#		else {next};
		}
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

close (OUTPUT);

