program WhisperBenchCli;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  WhisperCliMain in 'src\WhisperCliMain.pas',
  whisper in '..\..\..\src\whisper.pas',
  whisperexternal in '..\..\..\src\whisperexternal.pas',
  whispertypes in '..\..\..\src\whispertypes.pas',
  WhisperUtils in '..\..\..\src\WhisperUtils.pas',
  WhisperDynlib in '..\..\..\src\WhisperDynlib.pas',
  ggmltypes in '..\..\..\src\ggmltypes.pas',
  WhisperPlatform in '..\..\..\src\WhisperPlatform.pas',
  ggmlexternal in '..\..\..\src\ggmlexternal.pas',
  WhisperLog in '..\..\..\src\WhisperLog.pas',
  Quick.Logger in '..\..\..\thirdparty\QuickLogger\Quick.Logger.pas',
  Quick.Logger.Provider.Console in '..\..\..\thirdparty\QuickLogger\Quick.Logger.Provider.Console.pas',
  Quick.Logger.Provider.Files in '..\..\..\thirdparty\QuickLogger\Quick.Logger.Provider.Files.pas',
  Quick.Json.Serializer in '..\..\..\thirdparty\QuickLib\Quick.Json.Serializer.pas',
  Quick.JSON.Utils in '..\..\..\thirdparty\QuickLib\Quick.JSON.Utils.pas',
  Quick.Log in '..\..\..\thirdparty\QuickLib\Quick.Log.pas',
  Quick.Logger.Intf in '..\..\..\thirdparty\QuickLib\Quick.Logger.Intf.pas',
  Quick.RTTI.Utils in '..\..\..\thirdparty\QuickLib\Quick.RTTI.Utils.pas',
  Quick.Serializer.Intf in '..\..\..\thirdparty\QuickLib\Quick.Serializer.Intf.pas',
  Quick.SysInfo in '..\..\..\thirdparty\QuickLib\Quick.SysInfo.pas',
  Quick.Threads in '..\..\..\thirdparty\QuickLib\Quick.Threads.pas',
  Quick.Value in '..\..\..\thirdparty\QuickLib\Quick.Value.pas',
  Quick.Base64 in '..\..\..\thirdparty\QuickLib\Quick.Base64.pas',
  Quick.Commons in '..\..\..\thirdparty\QuickLib\Quick.Commons.pas',
  Quick.Console in '..\..\..\thirdparty\QuickLib\Quick.Console.pas',
  Quick.FaultControl in '..\..\..\thirdparty\QuickLib\Quick.FaultControl.pas';

{$R *.res}
begin
    { TODO -oUser -cConsole Main : Insert code here }
  WhisperMain;
end.
