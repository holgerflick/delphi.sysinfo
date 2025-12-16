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
    SysInfo1: TSysInfo;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
  private
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
  SysInfo1.RefreshAll;
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
  MemoHardware.Lines.Add('Name: ' + SysInfo1.CPUName);
  MemoHardware.Lines.Add('Vendor: ' + SysInfo1.CPUVendor);
  MemoHardware.Lines.Add('Identifier: ' + SysInfo1.CPUIdentifier);
  MemoHardware.Lines.Add('Speed: ' + IntToStr(SysInfo1.CPUSpeed) + ' MHz');
  MemoHardware.Lines.Add('Count: ' + IntToStr(SysInfo1.CPUCount) + ' processor(s)');
  MemoHardware.Lines.Add('Architecture: ' + SysInfo1.CPUArchitecture);
  MemoHardware.Lines.Add('');

  MemoHardware.Lines.Add('=== MEMORY INFORMATION ===');
  MemoHardware.Lines.Add('Total Memory: ' + Format('%.2f GB', [SysInfo1.TotalMemory / (1024*1024*1024)]));
  MemoHardware.Lines.Add('Available Memory: ' + Format('%.2f GB', [SysInfo1.AvailableMemory / (1024*1024*1024)]));
  MemoHardware.Lines.Add('Memory Load: ' + IntToStr(SysInfo1.MemoryLoad) + '%');
  MemoHardware.Lines.Add('Page File Total: ' + Format('%.2f GB', [SysInfo1.PageFileTotal / (1024*1024*1024)]));
  MemoHardware.Lines.Add('Page File Free: ' + Format('%.2f GB', [SysInfo1.PageFileFree / (1024*1024*1024)]));
  MemoHardware.Lines.Add('');

  MemoHardware.Lines.Add('=== MAINBOARD INFORMATION ===');
  MemoHardware.Lines.Add('Manufacturer: ' + SysInfo1.MainboardManufacturer);
  MemoHardware.Lines.Add('Product: ' + SysInfo1.MainboardProduct);
  MemoHardware.Lines.Add('BIOS Vendor: ' + SysInfo1.BIOSVendor);
  MemoHardware.Lines.Add('BIOS Version: ' + SysInfo1.BIOSVersion);
  MemoHardware.Lines.Add('BIOS Date: ' + SysInfo1.BIOSDate);
end;

procedure TFormMain.DisplayDirectoriesInfo;
begin
  MemoDirectories.Lines.Clear;
  MemoDirectories.Lines.Add('=== SYSTEM DIRECTORIES ===');
  MemoDirectories.Lines.Add('Windows: ' + SysInfo1.WindowsDirectory);
  MemoDirectories.Lines.Add('System: ' + SysInfo1.SystemDirectory);
  MemoDirectories.Lines.Add('Temp: ' + SysInfo1.TempDirectory);
  MemoDirectories.Lines.Add('Program Files: ' + SysInfo1.ProgramFilesDirectory);
  MemoDirectories.Lines.Add('Program Files (x86): ' + SysInfo1.ProgramFilesX86Directory);
  MemoDirectories.Lines.Add('Common Files: ' + SysInfo1.CommonFilesDirectory);
  MemoDirectories.Lines.Add('');

  MemoDirectories.Lines.Add('=== USER DIRECTORIES ===');
  MemoDirectories.Lines.Add('Desktop: ' + SysInfo1.DesktopDirectory);
  MemoDirectories.Lines.Add('Documents: ' + SysInfo1.DocumentsDirectory);
  MemoDirectories.Lines.Add('Music: ' + SysInfo1.MusicDirectory);
  MemoDirectories.Lines.Add('Pictures: ' + SysInfo1.PicturesDirectory);
  MemoDirectories.Lines.Add('Videos: ' + SysInfo1.VideosDirectory);
  MemoDirectories.Lines.Add('Downloads: ' + SysInfo1.DownloadsDirectory);
  MemoDirectories.Lines.Add('Start Menu: ' + SysInfo1.StartMenuDirectory);
  MemoDirectories.Lines.Add('Startup: ' + SysInfo1.StartupDirectory);
  MemoDirectories.Lines.Add('');

  MemoDirectories.Lines.Add('=== APPDATA DIRECTORIES ===');
  MemoDirectories.Lines.Add('AppData (Roaming): ' + SysInfo1.AppDataDirectory);
  MemoDirectories.Lines.Add('Local AppData: ' + SysInfo1.LocalAppDataDirectory);
  MemoDirectories.Lines.Add('Common AppData: ' + SysInfo1.CommonAppDataDirectory);
end;

procedure TFormMain.DisplayDisksInfo;
var
  I: Integer;
begin
  MemoDisks.Lines.Clear;
  MemoDisks.Lines.Add('=== DISK INFORMATION ===');
  MemoDisks.Lines.Add('Found ' + IntToStr(SysInfo1.DiskList.Count) + ' drive(s):');
  MemoDisks.Lines.Add('');

  for I := 0 to SysInfo1.DiskList.Count - 1 do
  begin
    with SysInfo1.DiskList[I] do
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
  MemoUser.Lines.Add('User Name: ' + SysInfo1.UserName);
  MemoUser.Lines.Add('Computer Name: ' + SysInfo1.ComputerName);
  MemoUser.Lines.Add('User Domain: ' + SysInfo1.UserDomain);
  MemoUser.Lines.Add('User Directory: ' + SysInfo1.UserDirectory);
  MemoUser.Lines.Add('');
  MemoUser.Lines.Add('Running as Administrator: ' + BoolToStr(SysInfo1.IsAdmin, True));
end;

procedure TFormMain.DisplaySystemInfo;
var
  Uptime: Int64;
  Days, Hours, Minutes, Seconds: Integer;
begin
  MemoSystem.Lines.Clear;
  MemoSystem.Lines.Add('=== OPERATING SYSTEM ===');
  MemoSystem.Lines.Add('Product Name: ' + SysInfo1.WindowsProductName);
  MemoSystem.Lines.Add('Version: ' + SysInfo1.WindowsVersion);
  MemoSystem.Lines.Add('Build: ' + SysInfo1.WindowsBuild);
  MemoSystem.Lines.Add('64-bit OS: ' + BoolToStr(SysInfo1.Is64BitOS, True));
  MemoSystem.Lines.Add('64-bit Process: ' + BoolToStr(SysInfo1.Is64BitProcess, True));
  MemoSystem.Lines.Add('');

  MemoSystem.Lines.Add('=== SYSTEM STATUS ===');
  Uptime := SysInfo1.SystemUptime div 1000;
  Days := Uptime div 86400;
  Hours := (Uptime mod 86400) div 3600;
  Minutes := (Uptime mod 3600) div 60;
  Seconds := Uptime mod 60;

  MemoSystem.Lines.Add(Format('System Uptime: %d days, %d hours, %d minutes, %d seconds',
    [Days, Hours, Minutes, Seconds]));
end;

end.
