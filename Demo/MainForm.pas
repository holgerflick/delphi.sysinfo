unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  SysInfo;

type
  TFormMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    MemoHardware: TMemo;
    MemoDirectories: TMemo;
    MemoDisks: TMemo;
    MemoUser: TMemo;
    MemoSystem: TMemo;
    ButtonRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
  private
    FSysInfo: TSysInfo;
    procedure DisplayHardwareInfo;
    procedure DisplayDirectoriesInfo;
    procedure DisplayDisksInfo;
    procedure DisplayUserInfo;
    procedure DisplaySystemInfo;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FSysInfo := TSysInfo.Create(Self);
  Caption := 'System Information Demo';

  PageControl1.ActivePageIndex := 0;

  DisplayHardwareInfo;
  DisplayDirectoriesInfo;
  DisplayDisksInfo;
  DisplayUserInfo;
  DisplaySystemInfo;
end;

procedure TFormMain.ButtonRefreshClick(Sender: TObject);
begin
  FSysInfo.RefreshAll;
  DisplayHardwareInfo;
  DisplayDirectoriesInfo;
  DisplayDisksInfo;
  DisplayUserInfo;
  DisplaySystemInfo;
end;

procedure TFormMain.DisplayHardwareInfo;
begin
  MemoHardware.Lines.Clear;
  MemoHardware.Lines.Add('=== CPU INFORMATION ===');
  MemoHardware.Lines.Add('Name: ' + FSysInfo.CPUName);
  MemoHardware.Lines.Add('Vendor: ' + FSysInfo.CPUVendor);
  MemoHardware.Lines.Add('Identifier: ' + FSysInfo.CPUIdentifier);
  MemoHardware.Lines.Add('Speed: ' + IntToStr(FSysInfo.CPUSpeed) + ' MHz');
  MemoHardware.Lines.Add('Count: ' + IntToStr(FSysInfo.CPUCount) + ' processor(s)');
  MemoHardware.Lines.Add('Architecture: ' + FSysInfo.CPUArchitecture);
  MemoHardware.Lines.Add('');

  MemoHardware.Lines.Add('=== MEMORY INFORMATION ===');
  MemoHardware.Lines.Add('Total Memory: ' + Format('%.2f GB', [FSysInfo.TotalMemory / (1024*1024*1024)]));
  MemoHardware.Lines.Add('Available Memory: ' + Format('%.2f GB', [FSysInfo.AvailableMemory / (1024*1024*1024)]));
  MemoHardware.Lines.Add('Memory Load: ' + IntToStr(FSysInfo.MemoryLoad) + '%');
  MemoHardware.Lines.Add('Page File Total: ' + Format('%.2f GB', [FSysInfo.PageFileTotal / (1024*1024*1024)]));
  MemoHardware.Lines.Add('Page File Free: ' + Format('%.2f GB', [FSysInfo.PageFileFree / (1024*1024*1024)]));
  MemoHardware.Lines.Add('');

  MemoHardware.Lines.Add('=== MAINBOARD INFORMATION ===');
  MemoHardware.Lines.Add('Manufacturer: ' + FSysInfo.MainboardManufacturer);
  MemoHardware.Lines.Add('Product: ' + FSysInfo.MainboardProduct);
  MemoHardware.Lines.Add('BIOS Vendor: ' + FSysInfo.BIOSVendor);
  MemoHardware.Lines.Add('BIOS Version: ' + FSysInfo.BIOSVersion);
  MemoHardware.Lines.Add('BIOS Date: ' + FSysInfo.BIOSDate);
end;

procedure TFormMain.DisplayDirectoriesInfo;
begin
  MemoDirectories.Lines.Clear;
  MemoDirectories.Lines.Add('=== SYSTEM DIRECTORIES ===');
  MemoDirectories.Lines.Add('Windows: ' + FSysInfo.WindowsDirectory);
  MemoDirectories.Lines.Add('System: ' + FSysInfo.SystemDirectory);
  MemoDirectories.Lines.Add('Temp: ' + FSysInfo.TempDirectory);
  MemoDirectories.Lines.Add('Program Files: ' + FSysInfo.ProgramFilesDirectory);
  MemoDirectories.Lines.Add('Program Files (x86): ' + FSysInfo.ProgramFilesX86Directory);
  MemoDirectories.Lines.Add('Common Files: ' + FSysInfo.CommonFilesDirectory);
  MemoDirectories.Lines.Add('');

  MemoDirectories.Lines.Add('=== USER DIRECTORIES ===');
  MemoDirectories.Lines.Add('Desktop: ' + FSysInfo.DesktopDirectory);
  MemoDirectories.Lines.Add('Documents: ' + FSysInfo.DocumentsDirectory);
  MemoDirectories.Lines.Add('Music: ' + FSysInfo.MusicDirectory);
  MemoDirectories.Lines.Add('Pictures: ' + FSysInfo.PicturesDirectory);
  MemoDirectories.Lines.Add('Videos: ' + FSysInfo.VideosDirectory);
  MemoDirectories.Lines.Add('Downloads: ' + FSysInfo.DownloadsDirectory);
  MemoDirectories.Lines.Add('Start Menu: ' + FSysInfo.StartMenuDirectory);
  MemoDirectories.Lines.Add('Startup: ' + FSysInfo.StartupDirectory);
  MemoDirectories.Lines.Add('');

  MemoDirectories.Lines.Add('=== APPDATA DIRECTORIES ===');
  MemoDirectories.Lines.Add('AppData (Roaming): ' + FSysInfo.AppDataDirectory);
  MemoDirectories.Lines.Add('Local AppData: ' + FSysInfo.LocalAppDataDirectory);
  MemoDirectories.Lines.Add('Common AppData: ' + FSysInfo.CommonAppDataDirectory);
end;

procedure TFormMain.DisplayDisksInfo;
var
  I: Integer;
begin
  MemoDisks.Lines.Clear;
  MemoDisks.Lines.Add('=== DISK INFORMATION ===');
  MemoDisks.Lines.Add('Found ' + IntToStr(FSysInfo.DiskList.Count) + ' drive(s):');
  MemoDisks.Lines.Add('');

  for I := 0 to FSysInfo.DiskList.Count - 1 do
  begin
    with FSysInfo.DiskList[I] do
    begin
      MemoDisks.Lines.Add('Drive ' + DriveLetter + ':\');
      MemoDisks.Lines.Add('  Type: ' + DriveType);
      MemoDisks.Lines.Add('  Label: ' + VolumeLabel);
      MemoDisks.Lines.Add('  File System: ' + FileSystem);
      MemoDisks.Lines.Add('  Serial Number: ' + IntToHex(SerialNumber, 8));
      MemoDisks.Lines.Add('  Total Space: ' + Format('%.2f GB', [TotalSpace / (1024*1024*1024)]));
      MemoDisks.Lines.Add('  Free Space: ' + Format('%.2f GB', [FreeSpace / (1024*1024*1024)]));
      MemoDisks.Lines.Add('  Used Space: ' + Format('%.2f GB', [(TotalSpace - FreeSpace) / (1024*1024*1024)]));
      if TotalSpace > 0 then
        MemoDisks.Lines.Add('  Usage: ' + Format('%.1f%%', [((TotalSpace - FreeSpace) / TotalSpace) * 100]));
      MemoDisks.Lines.Add('');
    end;
  end;
end;

procedure TFormMain.DisplayUserInfo;
begin
  MemoUser.Lines.Clear;
  MemoUser.Lines.Add('=== USER INFORMATION ===');
  MemoUser.Lines.Add('User Name: ' + FSysInfo.UserName);
  MemoUser.Lines.Add('Computer Name: ' + FSysInfo.ComputerName);
  MemoUser.Lines.Add('User Domain: ' + FSysInfo.UserDomain);
  MemoUser.Lines.Add('User Directory: ' + FSysInfo.UserDirectory);
  MemoUser.Lines.Add('');
  MemoUser.Lines.Add('Running as Administrator: ' + BoolToStr(FSysInfo.IsAdmin, True));
end;

procedure TFormMain.DisplaySystemInfo;
var
  Uptime: Int64;
  Days, Hours, Minutes, Seconds: Integer;
begin
  MemoSystem.Lines.Clear;
  MemoSystem.Lines.Add('=== OPERATING SYSTEM ===');
  MemoSystem.Lines.Add('Product Name: ' + FSysInfo.WindowsProductName);
  MemoSystem.Lines.Add('Version: ' + FSysInfo.WindowsVersion);
  MemoSystem.Lines.Add('Build: ' + FSysInfo.WindowsBuild);
  MemoSystem.Lines.Add('64-bit OS: ' + BoolToStr(FSysInfo.Is64BitOS, True));
  MemoSystem.Lines.Add('64-bit Process: ' + BoolToStr(FSysInfo.Is64BitProcess, True));
  MemoSystem.Lines.Add('');

  MemoSystem.Lines.Add('=== SYSTEM STATUS ===');
  Uptime := FSysInfo.SystemUptime div 1000;
  Days := Uptime div 86400;
  Hours := (Uptime mod 86400) div 3600;
  Minutes := (Uptime mod 3600) div 60;
  Seconds := Uptime mod 60;

  MemoSystem.Lines.Add(Format('System Uptime: %d days, %d hours, %d minutes, %d seconds',
    [Days, Hours, Minutes, Seconds]));
end;

end.
