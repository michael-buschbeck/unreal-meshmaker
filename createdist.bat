@echo off

echo Making distribution package...

C:
cd "C:\Programme\Spiele\Unreal Tournament"

if not exist MeshMaker                          mkdir MeshMaker
if not exist MeshMaker\Installer                mkdir MeshMaker\Installer
if not exist MeshMaker\Installer\Help           mkdir MeshMaker\Installer\Help
if not exist MeshMaker\Installer\Help\MeshMaker mkdir MeshMaker\Installer\Help\MeshMaker
if not exist MeshMaker\Installer\Maps           mkdir MeshMaker\Installer\Maps
if not exist MeshMaker\Installer\System         mkdir MeshMaker\Installer\System
if not exist Help\MeshMaker                     mkdir Help\MeshMaker

del /q MeshMaker
del /q MeshMaker\Installer\Help
del /q MeshMaker\Installer\Help\MeshMaker
del /q MeshMaker\Installer\Maps
del /q MeshMaker\Installer\System
del /q Help\MeshMaker

copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MeshMakerMOD.*" System

copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MeshMaker.bmp"  MeshMaker
copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MeshMaker.txt"  MeshMaker\Installer\Help
copy "D:\Daten\Internet\Html\Projekte\PlanetUnreal\screen\meshmaker"            MeshMaker\Installer\Help\MeshMaker
copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MyLamp.t3d"     MeshMaker\Installer\Maps
copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MeshMaker.exe"  MeshMaker\Installer\System

copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MeshMaker.txt"  Help
copy "D:\Daten\Internet\Html\Projekte\PlanetUnreal\screen\meshmaker"            Help\MeshMaker
copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MyLamp.t3d"     Maps
copy "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker\MeshMaker.exe"  System

cd "C:\Programme\Spiele\Unreal Tournament\MeshMaker\Installer"
zip -u -r -9 ..\MeshMaker-zip.zip .

cd "C:\Programme\Spiele\Unreal Tournament\System"
copy Manifest.ini Manifest-backup.ini
ucc master MeshMakerMOD
move Manifest-backup.ini Manifest.ini
del Manifest.int
zip -j -u -9 ..\MeshMaker\MeshMaker-umod.zip MeshMaker.umod ..\MeshMaker\Installer\Help\MeshMaker.txt

cd "C:\Programme\Spiele\Unreal Tournament"
copy MeshMaker\MeshMaker-zip.zip  "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker"
copy MeshMaker\MeshMaker-umod.zip "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi\MeshMaker"

D:
cd "D:\Daten\Programmentwicklung\(Sonstiges)\Delphi"

echo Done.
