{
  Developer: Halil Han BADEM.
  Github: https://github.com/halilhanbadem
  Release Date: 15/07/2021
  Note; It is free and exchangeable. Please, if you are going to use the codes in an article, I would like you to cite the source.
  For collaborations: halilhanbadem@gmail.com
  Website: halilhanbadem.dev
}

unit uSplash;

interface

 uses ToolsAPI, Vcl.Graphics;

 procedure Register;

implementation

{$R ide.res}

procedure SplashText;
var
 bim: TBitmap;
begin
 if SplashScreenServices = nil then
 begin
   exit;
 end;
 bim := TBitmap.Create;
 try
  bim.LoadFromResourceName(HInstance, 'IDEM');
  SplashScreenServices.AddPluginBitmap('H "Han" B - ', bim.Handle, False, 'Licensed', 'Hobby Component Set');
 finally
   bim.Free;
 end;
end;


procedure register;
begin
 SplashText;
end;
end.
