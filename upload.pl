eval {
  print "Uploading files to Link-M...\n";
  system 'scp MeshMaker*.zip michael@merkur.link-m.de:/usr/local/apache/vhosts/mb/html-data/download';
  
  print "Uploading files to FilePlanet...\n";
  open FTP, '| ftp -n -i www.planetunreal.com'
    or die "Unable to open FTP to www.planetunreal.com\n";
  print FTP "user mychaeel 2rj8n1\n";
  print FTP "put MeshMaker-umod.zip /cdrom/screen/MeshMaker-umod.zip\n";
  print FTP "put MeshMaker-zip.zip /cdrom/screen/MeshMaker-zip.zip\n";
  print FTP "quit\n";
  close FTP;
  };

print "Done. (Press Enter to continue.)\n";
<STDIN>;