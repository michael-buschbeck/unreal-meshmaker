unit GraphTools;


interface


uses
  Windows, SysUtils, Graphics, Pcx, Targa;


(*****************************************************************************)
(*  BitmapFileInfo
(*  BitmapFileLoad
(*  BitmapFileSave
(*****************************************************************************)

type
  TBitmapFormat =
    (bfUnknown,
     bfBmp,
     bfPcx,
     bfTga);

  TBitmapFileInfo = record
    FormatBitmap: TBitmapFormat;
    FormatPixel:  TPixelFormat;
    Width:  Cardinal;
    Height: Cardinal;
  end;


function BitmapFileInfo(PointerHeader: Pointer): TBitmapFileInfo; overload;
function BitmapFileInfo(TextFileBitmap: string): TBitmapFileInfo; overload;

procedure BitmapFileLoad(TextFileBitmap: string; var BitmapTarget: TBitmap);
procedure BitmapFileSave(TextFileBitmap: string; BitmapSource: TBitmap; FormatBitmap: TBitmapFormat = bfUnknown);


(*****************************************************************************)
(*  BitmapShrink
(*****************************************************************************)

type
  EBitmapShrink = class(Exception);
  EBitmapShrinkInvalidFactor = class(EBitmapShrink);
  EBitmapShrinkInvalidSource = class(EBitmapShrink);


procedure BitmapShrink(BitmapSource: TBitmap; var BitmapTarget: TBitmap; FactorShrink: Integer);


(*****************************************************************************)
(*  BitmapPalettize
(*****************************************************************************)

type
  EBitmapPalettize = class(Exception);
  EBitmapPalettizeInvalidSource = class(EBitmapPalettize);

  
procedure BitmapPalettize(BitmapSource: TBitmap; var BitmapTarget: TBitmap; CountColors: Cardinal = 256);


implementation


(*****************************************************************************)
(*  BitmapFileInfo
(*****************************************************************************)

type
  PBitmapInfoBmp = ^TBitmapInfoBmp;
  TBitmapInfoBmp = record
    bfType:          array [0..1] of Char;
    bfSize:          Longword;
    bfReserved1:     Word;
    bfReserved2:     Word;
    bfOffBits:       Longword;

    biSize:          Longword;
    biWidth:         Longword;
    biHeight:        Longword;
    biPlanes:        Word;
    biBitCount:      Word;
    biCompression:   Longword;
    biSizeImage:     Longword;
    biXPelsPerMeter: Longword;
    biYPelsPerMeter: Longword;
    biClrUsed:       Longword;
    biClrImportant:  Longword;
  end;

  PBitmapInfoPcx = ^TBitmapInfoPcx;
  TBitmapInfoPcx = record
    Manufacturer:    Byte;
    Version:         Byte;
    Encoding:        Byte;
    BitsPerPixel:    Byte;
    Window:          array [0..3] of Word;
    HDpi:            Word;
    VDpi:            Word;
    Colormap:        array [0..47] of Byte;
    Reserved:        Byte;
    NPlanes:         Byte;
    BytesPerLine:    Word;
    PaletteInfo:     Word;
    HScreenSize:     Word;
    VScreenSize:     Word;
    Filler:          array [0..53] of Byte;    
  end;

  PBitmapInfoTga = ^TBitmapInfoTga;
  TBitmapInfoTga = record
    IdentSize:       Byte;
    ColorMapType:    Byte;
    ImageType:       Byte;
    ColorMapStart:   Word;
    ColorMapLength:  Word;
    ColorMapBits:    Byte;
    XStart:          Word;
    YStart:          Word;
    Width:           Word;
    Height:          Word;
    Bits:            Byte;
    Descriptor:      Byte;
  end;


function BitmapFileInfo(PointerHeader: Pointer): TBitmapFileInfo;
begin
  if     (PBitmapInfoBmp(PointerHeader).bfType = 'BM')
     and (PBitmapInfoBmp(PointerHeader).biCompression in [0, 1, 2, 3])
     and (PBitmapInfoBmp(PointerHeader).biBitCount    in [1, 4, 8, 24]) then
  begin
    Result.FormatBitmap := bfBmp;
    Result.Width  := PBitmapInfoBmp(PointerHeader).biWidth;
    Result.Height := PBitmapInfoBmp(PointerHeader).biHeight;

    case PBitmapInfoBmp(PointerHeader).biBitCount of
       1:  Result.FormatPixel := pf1bit;
       4:  Result.FormatPixel := pf4bit;
       8:  Result.FormatPixel := pf8bit;
      24:  Result.FormatPixel := pf24bit;
    end;

    Exit;
  end;

  if     (PBitmapInfoPcx(PointerHeader).Manufacturer = 10)
     and (PBitmapInfoPcx(PointerHeader).Version      in [0, 2, 3, 4, 5])
     and (PBitmapInfoPcx(PointerHeader).Encoding     in [0, 1])
     and (PBitmapInfoPcx(PointerHeader).BitsPerPixel in [1, 2, 4, 8]) then
  begin
    Result.FormatBitmap := bfPcx;
    Result.Width  := PBitmapInfoPcx(PointerHeader).Window[2] - PBitmapInfoPcx(PointerHeader).Window[0];
    Result.Height := PBitmapInfoPcx(PointerHeader).Window[3] - PBitmapInfoPcx(PointerHeader).Window[1];

    case PBitmapInfoPcx(PointerHeader).BitsPerPixel * PBitmapInfoPcx(PointerHeader).NPlanes of
       1:  Result.FormatPixel := pf1bit;
       4:  Result.FormatPixel := pf4bit;
       8:  Result.FormatPixel := pf8bit;
      24:  Result.FormatPixel := pf24bit;
    end;

    Exit;
  end;

  if     (PBitmapInfoTga(PointerHeader).ImageType in [1, 2, 4, 9, 10, 11])
     and (PBitmapInfoTga(PointerHeader).Bits      in [1, 4, 8, 16, 24, 32]) then
  begin
    Result.FormatBitmap := bfTga;
    Result.Width  := PBitmapInfoTga(PointerHeader).Width;
    Result.Height := PBitmapInfoTga(PointerHeader).Height;

    case PBitmapInfoTga(PointerHeader).Bits of
       1:  Result.FormatPixel := pf1bit;
       4:  Result.FormatPixel := pf4bit;
       8:  Result.FormatPixel := pf8bit;
      16:  Result.FormatPixel := pf16bit;
      24:  Result.FormatPixel := pf24bit;
      32:  Result.FormatPixel := pf32bit;
    end;

    Exit;
  end;

  Result.FormatBitmap := bfUnknown;
  Result.FormatPixel  := pfCustom;
  Result.Width  := 0;
  Result.Height := 0;
end;


function BitmapFileInfo(TextFileBitmap: string): TBitmapFileInfo;
var
  BufferHeader: array [0..127] of Byte;
  FileBitmap: file of Byte;
begin
  AssignFile(FileBitmap, TextFileBitmap);
  Reset(FileBitmap);

  try
    Seek(FileBitmap, 0);
    BlockRead(FileBitmap, BufferHeader, SizeOf(BufferHeader));
    Result := BitmapFileInfo(Addr(BufferHeader));

  finally
    CloseFile(FileBitmap);
  end;
end;


(*****************************************************************************)
(*  BitmapFileLoad
(*****************************************************************************)

procedure BitmapFileLoad(TextFileBitmap: string; var BitmapTarget: TBitmap);
var
  InfoBitmap: TBitmapFileInfo;
begin
  InfoBitmap := BitmapFileInfo(TextFileBitmap);

  if InfoBitmap.FormatBitmap = bfUnknown
    then raise EInvalidGraphic.Create('Unsupported image format.');

  if not Assigned(BitmapTarget)
    then BitmapTarget := TBitmap.Create;
  BitmapTarget.Width  := 0;
  BitmapTarget.Height := 0;

  case InfoBitmap.FormatBitmap of
    bfBmp:  BitmapTarget.LoadFromFile(TextFileBitmap);
    bfPcx:  Pcx  .LoadFromFileX(TextFileBitmap, BitmapTarget);
    bfTga:  Targa.LoadFromFileX(TextFileBitmap, BitmapTarget);
  end;

  if (BitmapTarget.Width = 0) or (BitmapTarget.Height = 0)
    then raise EInvalidGraphic.Create('Unable to load image file.');
end;


(*****************************************************************************)
(*  BitmapFileSave
(*****************************************************************************)

procedure BitmapFileSave(TextFileBitmap: string; BitmapSource: TBitmap; FormatBitmap: TBitmapFormat = bfUnknown);
var
  TextExtension: string;
  FormatColor: Byte;
begin
  if FormatBitmap = bfUnknown then
  begin
    TextExtension := LowerCase(ExtractFileExt(TextFileBitmap));

         if    (TextExtension = '.bmp')
            or (TextExtension = '.dib') then FormatBitmap := bfBmp
    else if    (TextExtension = '.pcx') then FormatBitmap := bfPcx
    else if    (TextExtension = '.tga') then FormatBitmap := bfTga
    else raise EInvalidGraphic.Create('Unable to determine target image format from file extension.');
  end;

  if not Assigned(BitmapSource)
    then raise EInvalidGraphic.Create('Source bitmap object is unassigned.');

  if BitmapSource.PixelFormat <= pf8bit
    then FormatColor := 1
    else FormatColor := 2;

  case FormatBitmap of
    bfBmp:  BitmapSource.SaveToFile(TextFileBitmap);
    bfPcx:  Pcx  .SaveToFileX(TextFileBitmap, BitmapSource, FormatColor);
    bfTga:  Targa.SaveToFileX(TextFileBitmap, BitmapSource, FormatColor);
  end;
end;


(*****************************************************************************)
(*  BitmapShrink
(*****************************************************************************)

procedure BitmapShrink(BitmapSource: TBitmap; var BitmapTarget: TBitmap; FactorShrink: Integer);
var
  CountBytesSkipCell: Integer;
  CountBytesSkipCellRow: Integer;
  CountBytesSkipRow: Integer;
  CountPixelsCell: Integer;
  IndexColCell: Integer;
  IndexColTarget: Integer;
  IndexRowCell: Integer;
  IndexRowTarget: Integer;
  PointerPixelSource: PByteArray;
  PointerPixelSourceCell: PByteArray;
  PointerPixelSourceEnd: PByteArray;
  PointerPixelTarget: PByteArray;
  SumColorRed: Integer;
  SumColorGreen: Integer;
  SumColorBlue: Integer;
begin
  if FactorShrink < 1
    then raise EBitmapShrinkInvalidFactor.Create('Shrink factor invalid');

  if not Assigned(BitmapSource)
    then raise EBitmapShrinkInvalidSource.Create('Source bitmap object unassigned');

  if (BitmapSource.Width  mod FactorShrink <> 0)
  or (BitmapSource.Height mod FactorShrink <> 0)
    then raise EBitmapShrinkInvalidSource.Create('Source bitmap size is not divisible by shrink factor');

  if BitmapSource.PixelFormat <> pf24bit
    then raise EBitmapShrinkInvalidSource.Create('Source bitmap is not TrueColor');

  if not Assigned(BitmapTarget)
    then BitmapTarget := TBitmap.Create;
  BitmapTarget.PixelFormat := pf24bit;
  BitmapTarget.Width  := BitmapSource.Width  div FactorShrink;
  BitmapTarget.Height := BitmapSource.Height div FactorShrink;

  CountBytesSkipCell := FactorShrink * 3;
  CountBytesSkipCellRow := (FactorShrink - 1) * BitmapSource.Width * 3;
  CountBytesSkipRow := (BitmapSource.Width - FactorShrink) * 3;

  CountPixelsCell := FactorShrink * FactorShrink;

  PointerPixelSource := BitmapSource.ScanLine[BitmapSource.Height - 1];
  PointerPixelSourceEnd := PByteArray(Cardinal(PointerPixelSource) + Cardinal(BitmapSource.Width) * Cardinal(BitmapSource.Height) * 3);
  PointerPixelTarget := BitmapTarget.ScanLine[BitmapTarget.Height - 1];

  while Cardinal(PointerPixelSource) < Cardinal(PointerPixelSourceEnd) do
  begin
    for IndexRowTarget := 1 to BitmapTarget.Height do
    begin
      for IndexColTarget := 1 to BitmapTarget.Width do
      begin
        PointerPixelSourceCell := PointerPixelSource;

        SumColorRed   := 0;
        SumColorGreen := 0;
        SumColorBlue  := 0;

        for IndexRowCell := 1 to FactorShrink do
        begin
          for IndexColCell := 1 to FactorShrink do
          begin
            Inc(SumColorRed,   PointerPixelSourceCell[2]);
            Inc(SumColorGreen, PointerPixelSourceCell[1]);
            Inc(SumColorBlue,  PointerPixelSourceCell[0]);
            Inc(Cardinal(PointerPixelSourceCell), 3);
          end;
          Inc(Cardinal(PointerPixelSourceCell), CountBytesSkipRow);
        end;

        PointerPixelTarget[2] := SumColorRed   div CountPixelsCell;
        PointerPixelTarget[1] := SumColorGreen div CountPixelsCell;
        PointerPixelTarget[0] := SumColorBlue  div CountPixelsCell;

        Inc(Cardinal(PointerPixelTarget), 3);
        Inc(Cardinal(PointerPixelSource), CountBytesSkipCell);
      end;

      Inc(Cardinal(PointerPixelSource), CountBytesSkipCellRow);
    end;
  end;
end;


(*****************************************************************************)
(*  BitmapPalettize
(*****************************************************************************)

type
  POctreeNode = ^TOctreeNode;

  TOctreeNode = record
    CountChildren:    Cardinal;
    CountPixels:      Cardinal;
    CountPixelsTotal: Cardinal;
    Error:            Cardinal;
    IndexEntry:       Cardinal;
    SumColorRed:      Cardinal;
    SumColorGreen:    Cardinal;
    SumColorBlue:     Cardinal;
    Nodes: array [0..7] of POctreeNode;
  end;


procedure BitmapPalettize(BitmapSource: TBitmap; var BitmapTarget: TBitmap; CountColors: Cardinal);
var
  CountEntries: Cardinal;
  ErrorMin: Cardinal;
  ErrorMinNext: Cardinal;
  IndexEntry: Cardinal;
  InfoPalette: PLogPalette;


  (*******************************************************)
  (*  BitmapPalettize / OctreePrune
  (*******************************************************)

  procedure OctreePrune(OctreeNode: POctreeNode; FlagPrune: Boolean = False);
  var
    IndexNode: Cardinal;
    OctreeNodeChild: POctreeNode;
  begin
    for IndexNode := 7 downto 0 do
    begin
      OctreeNodeChild := OctreeNode.Nodes[IndexNode];

      if Assigned(OctreeNodeChild) then
      begin
        if FlagPrune or (OctreeNodeChild.Error <= ErrorMin) then
        begin
          OctreePrune(OctreeNodeChild, True);

          if OctreeNode.CountPixels = 0 then Inc(CountEntries);

          Inc(OctreeNode.CountPixels,   OctreeNodeChild.CountPixels);
          Inc(OctreeNode.SumColorRed,   OctreeNodeChild.SumColorRed);
          Inc(OctreeNode.SumColorGreen, OctreeNodeChild.SumColorGreen);
          Inc(OctreeNode.SumColorBlue,  OctreeNodeChild.SumColorBlue);

          FreeMem(OctreeNodeChild);
          OctreeNode.Nodes[IndexNode] := nil;
          Dec(OctreeNode.CountChildren);
          Dec(CountEntries);
        end
        else begin
          OctreePrune(OctreeNodeChild, FlagPrune);
        end;
      end;
    end;

    if not FlagPrune or (OctreeNode.Error > ErrorMin) then
    begin
      if ErrorMinNext > OctreeNode.Error then ErrorMinNext := OctreeNode.Error;
    end;
  end;


  (*******************************************************)
  (*  BitmapPalettize / OctreeFree
  (*******************************************************)

  procedure OctreeFree(OctreeNode: POctreeNode);
  var
    IndexNode: Cardinal;
    OctreeNodeChild: POctreeNode;
  begin
    for IndexNode := 7 downto 0 do
    begin
      OctreeNodeChild := OctreeNode.Nodes[IndexNode];

      if Assigned(OctreeNodeChild) then
      begin
        OctreeFree(OctreeNodeChild);
        FreeMem(OctreeNodeChild);
      end;
    end;
  end;


  (*******************************************************)
  (*  BitmapPalettize / PaletteCreate
  (*******************************************************)

  procedure PaletteCreate(OctreeNode: POctreeNode);
  var
    IndexNode: Cardinal;
    OctreeNodeChild: POctreeNode;
  begin
    if OctreeNode.CountPixels > 0 then
    begin
      with InfoPalette.palPalEntry[IndexEntry] do
      begin
        peRed   := OctreeNode.SumColorRed   div OctreeNode.CountPixels;
        peGreen := OctreeNode.SumColorGreen div OctreeNode.CountPixels;
        peBlue  := OctreeNode.SumColorBlue  div OctreeNode.CountPixels;
        peFlags := 0;
      end;

      OctreeNode.IndexEntry := IndexEntry;
      Inc(IndexEntry);
    end;

    for IndexNode := 7 downto 0 do
    begin
      OctreeNodeChild := OctreeNode.Nodes[IndexNode];
      if Assigned(OctreeNodeChild) then PaletteCreate(OctreeNodeChild);
    end;
  end;


(*********************************************************)
(* BitmapPalettize / Main
(*********************************************************)

var
  ColorCenterDelta: Integer;
  ColorCenterRed: Integer;
  ColorCenterGreen: Integer;
  ColorCenterBlue: Integer;
  ColorDeltaRed: Integer;
  ColorDeltaGreen: Integer;
  ColorDeltaBlue: Integer;
  ColorPixel: TColor;
  ColorPixelRed: Integer;
  ColorPixelGreen: Integer;
  ColorPixelBlue: Integer;
  CountBytesPixel: Cardinal;
CountTicksStart: Cardinal;
CountTicksBuild: Cardinal;
CountTicksEnd: Cardinal;
  HandlePalette: HPalette;
  IndexBit: Cardinal;
  IndexNode: Cardinal;
  IndexPixel: Cardinal;
  IndexRow: Cardinal;
  OctreeNode: POctreeNode;
  OctreeNodeRoot: TOctreeNode;
  PointerPixelSource: PColor;
  PointerPixelTarget: PByte;
begin
CountTicksStart := GetTickCount;
  FillChar(OctreeNodeRoot, SizeOf(OctreeNodeRoot), 0);

  if not Assigned(BitmapSource)
    then raise EBitmapPalettizeInvalidSource.Create('Source bitmap object unassigned');
  if not (BitmapSource.PixelFormat in [pf24bit, pf32bit])
    then raise EBitmapPalettizeInvalidSource.Create('Source bitmap is not TrueColor');

  CountEntries := 0;

  case BitmapSource.PixelFormat of
    pf24bit:  CountBytesPixel := 3;
    pf32bit:  CountBytesPixel := 4;
    else      CountBytesPixel := 0;
  end;

  for IndexRow := BitmapSource.Height - 1 downto 0 do
  begin
    PointerPixelSource := BitmapSource.ScanLine[IndexRow];

    for IndexPixel := BitmapSource.Width - 1 downto 0 do
    begin
      ColorPixel := PointerPixelSource^;

      ColorPixelRed   := ColorPixel shr 16 and $ff;
      ColorPixelGreen := ColorPixel shr  8 and $ff;
      ColorPixelBlue  := ColorPixel        and $ff;

      ColorCenterRed   := 128;
      ColorCenterGreen := 128;
      ColorCenterBlue  := 128;
      ColorCenterDelta := 128;

      OctreeNode := Addr(OctreeNodeRoot);
      Inc(OctreeNodeRoot.CountPixelsTotal);

      for IndexBit := 7 downto 0 do
      begin
        IndexNode := ColorPixel shr IndexBit and $010101;

        IndexNode :=    (IndexNode        and $01)
                     or (IndexNode shr  7 and $02)
                     or (IndexNode shr 14 and $04);

        ColorDeltaRed   := ColorPixelRed   - ColorCenterRed;
        ColorDeltaGreen := ColorPixelGreen - ColorCenterGreen;
        ColorDeltaBlue  := ColorPixelBlue  - ColorCenterBlue;

        Inc(OctreeNode.Error, ColorDeltaRed   * ColorDeltaRed   +
                              ColorDeltaGreen * ColorDeltaGreen +
                              ColorDeltaBlue  * ColorDeltaBlue);

        ColorCenterDelta := ColorCenterDelta shr 1;
        if (IndexNode and $01) <> 0 then Inc(ColorCenterBlue,  ColorCenterDelta) else Dec(ColorCenterBlue,  ColorCenterDelta);
        if (IndexNode and $02) <> 0 then Inc(ColorCenterGreen, ColorCenterDelta) else Dec(ColorCenterGreen, ColorCenterDelta);
        if (IndexNode and $04) <> 0 then Inc(ColorCenterRed,   ColorCenterDelta) else Dec(ColorCenterRed,   ColorCenterDelta);

        if not Assigned(OctreeNode.Nodes[IndexNode]) then
        begin
          OctreeNode.Nodes[IndexNode] := AllocMem(SizeOf(TOctreeNode));
          Inc(OctreeNode.CountChildren);
        end;

        OctreeNode := OctreeNode.Nodes[IndexNode];
        Inc(OctreeNode.CountPixelsTotal);
      end;

      if OctreeNode.CountPixels = 0 then Inc(CountEntries);

      Inc(OctreeNode.CountPixels);
      Inc(OctreeNode.SumColorRed,   ColorPixelRed);
      Inc(OctreeNode.SumColorGreen, ColorPixelGreen);
      Inc(OctreeNode.SumColorBlue,  ColorPixelBlue);

      Inc(Cardinal(PointerPixelSource), CountBytesPixel);
    end;
  end;

CountTicksBuild := GetTickCount;

  ErrorMinNext := 0;

  while CountEntries > CountColors do
  begin
    ErrorMin := ErrorMinNext;
    ErrorMinNext := High(Cardinal);

    OctreePrune(Addr(OctreeNodeRoot));
  end;

  InfoPalette := AllocMem(SizeOf(TPaletteEntry) * CountEntries + 4);
  IndexEntry := 0;
  PaletteCreate(Addr(OctreeNodeRoot));
  InfoPalette.palVersion := $300;
  InfoPalette.palNumEntries := IndexEntry;
  HandlePalette := CreatePalette(InfoPalette^);
  FreeMem(InfoPalette);

  if not Assigned(BitmapTarget)
    then BitmapTarget := TBitmap.Create;
  BitmapTarget.PixelFormat := pf8bit;
  BitmapTarget.Width   := BitmapSource.Width;
  BitmapTarget.Height  := BitmapSource.Height;
  BitmapTarget.Palette := HandlePalette;

  for IndexRow := BitmapSource.Height - 1 downto 0 do
  begin
    PointerPixelSource := BitmapSource.ScanLine[IndexRow];
    PointerPixelTarget := BitmapTarget.ScanLine[IndexRow];

    for IndexPixel := BitmapSource.Width - 1 downto 0 do
    begin
      ColorPixel := PointerPixelSource^;

      OctreeNode := Addr(OctreeNodeRoot);

      for IndexBit := 7 downto 0 do
      begin
        IndexNode := ColorPixel shr IndexBit and $010101;

        IndexNode :=    (IndexNode        and $01)
                     or (IndexNode shr  7 and $02)
                     or (IndexNode shr 14 and $04);

        if not Assigned(OctreeNode.Nodes[IndexNode]) then Break;
        OctreeNode := OctreeNode.Nodes[IndexNode];
      end;

      PointerPixelTarget^ := OctreeNode.IndexEntry;

      Inc(Cardinal(PointerPixelSource), CountBytesPixel);
      Inc(Cardinal(PointerPixelTarget));
    end;
  end;

  OctreeFree(Addr(OctreeNodeRoot));
CountTicksEnd := GetTickCount;
BitmapTarget.Canvas.TextOut(10,10,Format('%d colors, %d entries, %d ms (%d ms build)', [IndexEntry, CountEntries, CountTicksEnd - CountTicksStart, CountTicksBuild - CountTicksStart]));
end;


end.
