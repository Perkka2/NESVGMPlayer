
<form action="<?php echo $_SERVER["PHP_SELF"]; ?>" method="post" enctype="multipart/form-data">

;Select VGM file to process (Must be .vgm, not .vgz) :<br/>
<input type="file" name="file" id="file" /><br/>
<input type="submit" name="submit" value="Process"/>

</form>

<?php
ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);
// get size of the binary file
if ( isset($_POST["submit"]) ) {

   if ( isset($_FILES["file"])) {

            //if there was an error uploading the file
        if ($_FILES["file"]["error"] > 0) {
            echo "Return Code: " . $_FILES["file"]["error"] . "<br />";

        }
        else {
                 //Print file details
             echo ";Upload: " . $_FILES["file"]["name"] . "<br />";
             //echo ";Type: " . $_FILES["file"]["type"] . "<br />";
             echo ";Size: " . ($_FILES["file"]["size"] / 1024) . " Kb<br /><br/>";
             //echo ";Temp file: " . $_FILES["file"]["tmp_name"] . "<br />";
            $storagename = "track.vgm";
            move_uploaded_file($_FILES["file"]["tmp_name"], $storagename);
            }

     } else {
             echo ";No file selected <br />";
     }
}

$track = 'track.vgm';
$output = 'track.txt';
$filesize = filesize($track);
// open file for reading in binary mode
$fp = fopen($track, 'rb');
$fpout = fopen($output, 'w');
// read the entire file into a binary string
$binary = fread($fp, $filesize);
// finally close the file
fclose($fp);

// unpack the data - notice that we create a format code using 'C%d'
// that will unpack the size of the file in unsigned chars
$unpacked = unpack(sprintf('C%d', $filesize), $binary);

// reset array keys
$unpacked = array_values($unpacked);

// this variable holds the size of *one* structure in the file
//$block_size = 3;
// figure out the number of blocks in the file
//$block_count = $filesize/$block_size;
function hex2str($hex) {
    $str = '';
    for($i=0;$i<strlen($hex);$i+=2) $str .= chr(hexdec(substr($hex,$i,2)));
    return $str;
}

// you now should have an array where each element represents a
// unsigned char from the binary file, so to display Day, Month and Year
$i = 52; //offset vgm data start
$datasize = 0;
$bank = 0;
$dataOffset = hexdec(dechex($unpacked[$i+3]).dechex($unpacked[$i+2]).dechex($unpacked[$i+1]).dechex($unpacked[$i+0]));
$GD3offset = 20+hexdec(dechex($unpacked[23]).dechex($unpacked[22]).dechex($unpacked[21]).dechex($unpacked[20]));
$GD3size = 12+hexdec(substr(0 . dechex($unpacked[$GD3offset+11]),-2).substr(0 . dechex($unpacked[$GD3offset+10]),-2).substr(0 . dechex($unpacked[$GD3offset+9]),-2).substr(0 . dechex($unpacked[$GD3offset+8]),-2));
//print substr(0 . dechex($unpacked[$GD3offset+11]),-2).substr(0 . dechex($unpacked[$GD3offset+10]),-2).substr(0 . dechex($unpacked[$GD3offset+9]),-2).substr(0 . dechex($unpacked[$GD3offset+8]),-2).'<br>';
//print $GD3size;

print "<br/>;";
fwrite($fpout,"\r\n;");
for($y=12;$y < ($GD3size);$y++){
  //print '<b>'.substr(0 . dechex($unpacked[$y+$GD3offset]),-2).'</b>';
  if(dechex($unpacked[$y+$GD3offset])!=0){
    //print hex2str(substr(0 . dechex($unpacked[$y+$GD3offset]),-2));
    $str = substr(0 . dechex($unpacked[$y+$GD3offset+1]),-2).substr(0 . dechex($unpacked[$y+$GD3offset]),-2);
    print mb_convert_encoding(hex2bin($str), 'UTF-8', 'UCS-2BE');
    fwrite($fpout,mb_convert_encoding(hex2bin($str), 'UTF-8', 'UCS-2BE'));
  }
  //if(dechex($unpacked[$y+$GD3offset+1])==0){print hex2str(substr(0 . dechex($unpacked[$y+$GD3offset]),-2).substr(0 . dechex($unpacked[$y+$GD3offset+1]),-2));}
  if($y+1 < ($GD3size)){
    if((dechex($unpacked[$y+$GD3offset]).dechex($unpacked[$y+$GD3offset+1]))==0){
      print "<br/>;";
      fwrite($fpout,"\r\n;");
    }
  }

}

$i = $i+$dataOffset;
/*print ';Start Offset: '.$i.'Data Offset: '.$dataOffset.'<br/>';*/
fwrite($fpout,';Start Offset: '.$i.'Data Offset: '.$dataOffset."\r\n");
while ($i < $filesize) {
/*	if(($bank == 0)&&($datasize > 7500)){
		print '&nbsp;&nbsp;&nbsp;&nbsp;jmp $8000<br/>';
		$bank++;
		print '.segment "BANK'.$bank.'"<br/><br/>';
		$datasize = 0;
	}
	else if ($datasize > 16000){
		print '&nbsp;&nbsp;&nbsp;&nbsp;jmp $8000<br/>';
		$bank++;
		print '.segment "BANK'.$bank.'"<br/><br/>';
		$datasize = 0;
	}*/
	if ($datasize > 7900){
		$bank++;/*
		print '
		<br/>&nbsp;&nbsp;&nbsp;&nbsp;lda #%0000011'. ($bank)%2 .'
	    <br/>&nbsp;&nbsp;&nbsp;&nbsp;STA MMC3_BANK_SELECT
		<br/>&nbsp;&nbsp;&nbsp;&nbsp;LDA #'.$bank.'
		<br/>&nbsp;&nbsp;&nbsp;&nbsp;STA MMC3_BANK_DATA
		<br/>';*/
	fwrite($fpout,'  lda #%0000011'. ($bank)%2 .'
  STA MMC3_BANK_SELECT
  LDA #'.$bank.'
  STA MMC3_BANK_DATA
	');
		if(($bank+1)%2){
			/*print '&nbsp;&nbsp;&nbsp;&nbsp;jmp $8000<br/>';*/
      fwrite($fpout,"jmp $8000\r\n");
		}
		else{
			/*print '&nbsp;&nbsp;&nbsp;&nbsp;jmp $A000<br/>';*/
      fwrite($fpout,'jmp $A000' . "\r\n");
		}
		if($bank < 10){
      /*print '<br/><br/>.segment "BANK0'.$bank.'"<br/><br/>';*/
      fwrite($fpout,'.segment "BANK0'.$bank.'"'."\r\n\r\n");
    }
		else {
      /*print '<br/><br/>.segment "BANK'.$bank.'"<br/><br/>';*/
      fwrite($fpout,'.segment "BANK'.$bank.'"'."\r\n\r\n");
      }
		$datasize = 0;
	}
   if($unpacked[$i] == hexdec(61)){
     $delayY = (41*(hexdec(dechex($unpacked[$i+2]) . dechex($unpacked[$i+1])))-15)/9%256;
     $delayA = floor((41*(hexdec(dechex($unpacked[$i+2]) . dechex($unpacked[$i+1])))-15)/9/256);
	 //print ';Delay Cycles: ' . hexdec(dechex($unpacked[$i+2]) . dechex($unpacked[$i+1])) . 'samples<br/>';
	 //print '&nbsp;&nbsp;&nbsp;&nbsp;LDY #'.$delayY.'<br/>&nbsp;&nbsp;&nbsp;&nbsp;LDA #'.$delayA.'<br />&nbsp;&nbsp;&nbsp;&nbsp;jsr delayloop<br /><br />';
   fwrite($fpout,'  LDY #'.$delayY."\r\n  LDA #".$delayA."\r\n  jsr delayloop\r\n\r\n");
	 $datasize+=7;
   }
   else if($unpacked[$i] == hexdec(62)){
	   //print '&nbsp;&nbsp;&nbsp;&nbsp;jsr delayloop735<br /><br />';
     fwrite($fpout,"  jsr delayloop735\r\n\r\n");
	   $i++;
	   $datasize+=3;
	   continue;
   }
   else if($unpacked[$i] == hexdec(63)){
	   //print '&nbsp;&nbsp;&nbsp;&nbsp;jsr delayloop882<br /><br />';
     fwrite($fpout," jsr delayloop882\r\n\r\n");
	   $i++;
	   $datasize+=3;
	   continue;
   }
   else if(($unpacked[$i] >= hexdec(70))&&($unpacked[$i] <= hexdec("7F"))){
	   //print '&nbsp;&nbsp;&nbsp;&nbsp;jsr delayloop'.($unpacked[$i]-hexdec("6F")) .'<br /><br />';
     fwrite($fpout,'  jsr delayloop'.($unpacked[$i]-hexdec("6F")) ."\r\n\r\n");
	   $i++;
	   $datasize+=3;
	   continue;
   }
   else if(($unpacked[$i] == hexdec(57))|($unpacked[$i] == hexdec(56))|($unpacked[$i] == hexdec('A0'))|($unpacked[$i] == hexdec(55))|($unpacked[$i] == hexdec(58))|($unpacked[$i] == hexdec(59))|($unpacked[$i] == hexdec(51))){
     $address = substr(0 . dechex($unpacked[$i+1]),-2);
     $data = substr(0 . dechex($unpacked[$i+2]),-2);
     //print '&nbsp;&nbsp;&nbsp;&nbsp;LDA #$' . strtoupper($address) . '  <br />';
     fwrite($fpout,'  LDA #$' . strtoupper($address) . "\r\n");
     //print '&nbsp;&nbsp;&nbsp;&nbsp;LDY #$' . strtoupper($data) . ' <br />';
     fwrite($fpout,'  LDY #$' . strtoupper($data) . "\r\n");
     if(($unpacked[$i] == hexdec(57))|($unpacked[$i] == hexdec(59))){
       //print '&nbsp;&nbsp;&nbsp;&nbsp;jsr sendtoYM2	<br /><br />';
       fwrite($fpout,"  jsr sendtoYM2\r\n\r\n");
     }
     else{
       //print '&nbsp;&nbsp;&nbsp;&nbsp;jsr sendtoYM	<br /><br />';
       fwrite($fpout,"  jsr sendtoYM\r\n\r\n");
     }
	 	 $datasize+=7;
   }
	 else if($unpacked[$i] == hexdec('B4')){
		 $address = substr(0 . dechex($unpacked[$i+1]),-2);
		 $data = substr(0 . dechex($unpacked[$i+2]),-2);
		 if($unpacked[$i+1] == hexdec('3F')){
			 $address = '23';
		 }
		 else if(($unpacked[$i+1] < hexdec('3F'))&($unpacked[$i+1] > hexdec('1F'))){
			 $address = dechex(hexdec($address) + hexdec('60')) ;
		 }
		 //print '&nbsp;&nbsp;&nbsp;&nbsp;LDA #$' . strtoupper($data) . ' <br />';
     fwrite($fpout,'  LDA #$' . strtoupper($data) . "\r\n");
		 //print '&nbsp;&nbsp;&nbsp;&nbsp;STA $40' . strtoupper($address) . '  <br /><br />';
     fwrite($fpout,'  STA $40' . strtoupper($address) . "\r\n\r\n");
		 $datasize+=5;
	 }
   else if($unpacked[$i] == hexdec(66)) {
		 //print ';end';
     fwrite($fpout,';end');
		 break;
   }
   else if($unpacked[$i] == hexdec(67)) {
		 $datablocksize = hexdec(substr(0 . dechex($unpacked[$i+6]),-2) . substr(0 . dechex($unpacked[$i+5]),-2) . substr(0 . dechex($unpacked[$i+4]),-2) . substr(0 . dechex($unpacked[$i+3]),-2));
		 //print ';datablock start: size:'.$datablocksize.'bytes (ignored)<br/>;';
     fwrite($fpout,';datablock start: size:'.$datablocksize."bytes (ignored)\r\n;");
     for($y=0;$y < ($datablocksize+7);$y++){
       //print substr(0 . dechex($unpacked[$y+$i]),-2);
       fwrite($fpout,substr(0 . dechex($unpacked[$y+$i]),-2));
       if((($y-7)%32)==0){
         //print '<br/>;';
         fwrite($fpout,"\r\n;");
       }
     }
		 //print '<br/>;datablock end<br/><br/>';
     fwrite($fpout,"\r\n;datablock end\r\n\r\n");
		 $i = $i + $datablocksize + 7;
		 //$i = $i + 7;
		 continue;
   }
	 //print ";".$i." " .substr(0 . dechex($unpacked[$i]),-2)."<br/>";
	$i+=3;
}
fclose($fpout);
print '<br/><a href="track.txt">VGM ASM</a>';
?>
