unit SysInfo;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Winapi.ShlObj, System.Win.Registry,
  Vcl.Forms;

type
  TDiskInfo = class(TPersistent)
  private
    FDriveLetter: Char;
    FVolumeLabel: string;
    FFileSystem: string;
    FDriveType: string;
    FTotalSpace: Int64;
    FFreeSpace: Int64;
    FSerialNumber: Cardinal;
  public
    property DriveLetter: Char read FDriveLetter;
    property VolumeLabel: string read FVolumeLabel;
    property FileSystem: string read FFileSystem;
    property DriveType: string read FDriveType;
    property TotalSpace: Int64 read FTotalSpace;
    property FreeSpace: Int64 read FFreeSpace;
    property SerialNumber: Cardinal read FSerialNumber;
  end;

  TDiskList = class(TObject)
  private
    FList: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TDiskInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TDiskInfo read GetItem; default;
  end;

  TSysInfo = class(TComponent)
  private
    FDiskList: TDiskList;
    function GetCPUName: string;
    function GetCPUVendor: string;
    function GetCPUIdentifier: string;
    function GetCPUSpeed: Integer;
    function GetCPUCount: Integer;
    function GetCPUArchitecture: string;
    function GetTotalMemory: Int64;
    function GetAvailableMemory: Int64;
    function GetMemoryLoad: Integer;
    function GetPageFileTotal: Int64;
    function GetPageFileFree: Int64;
    function GetMainboardManufacturer: string;
    function GetMainboardProduct: string;
    function GetBIOSVendor: string;
    function GetBIOSVersion: string;
    function GetBIOSDate: string;
    function GetWindowsDirectory: string;
    function GetSystemDirectory: string;
    function GetTempDirectory: string;
    function GetProgramFilesDirectory: string;
    function GetProgramFilesX86Directory: string;
    function GetCommonFilesDirectory: string;
    function GetDesktopDirectory: string;
    function GetDocumentsDirectory: string;
    function GetMusicDirectory: string;
    function GetPicturesDirectory: string;
    function GetVideosDirectory: string;
    function GetDownloadsDirectory: string;
    function GetStartMenuDirectory: string;
    function GetStartupDirectory: string;
    function GetAppDataDirectory: string;
    function GetLocalAppDataDirectory: string;
    function GetCommonAppDataDirectory: string;
    function GetUserName: string;
    function GetUserDirectory: string;
    function GetComputerName: string;
    function GetUserDomain: string;
    function GetWindowsVersion: string;
    function GetWindowsBuild: string;
    function GetWindowsProductName: string;
    function GetSystemUptime: Int64;
    function GetIsAdmin: Boolean;
    function GetIs64BitOS: Boolean;
    function GetIs64BitProcess: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshAll;
    property DiskList: TDiskList read FDiskList;
  published
    property CPUName: string read GetCPUName stored False;
    property CPUVendor: string read GetCPUVendor stored False;
    property CPUIdentifier: string read GetCPUIdentifier stored False;
    property CPUSpeed: Integer read GetCPUSpeed stored False;
    property CPUCount: Integer read GetCPUCount stored False;
    property CPUArchitecture: string read GetCPUArchitecture stored False;

    property TotalMemory: Int64 read GetTotalMemory stored False;
    property AvailableMemory: Int64 read GetAvailableMemory stored False;
    property MemoryLoad: Integer read GetMemoryLoad stored False;
    property PageFileTotal: Int64 read GetPageFileTotal stored False;
    property PageFileFree: Int64 read GetPageFileFree stored False;

    property MainboardManufacturer: string read GetMainboardManufacturer stored False;
    property MainboardProduct: string read GetMainboardProduct stored False;
    property BIOSVendor: string read GetBIOSVendor stored False;
    property BIOSVersion: string read GetBIOSVersion stored False;
    property BIOSDate: string read GetBIOSDate stored False;

    property WindowsDirectory: string read GetWindowsDirectory stored False;
    property SystemDirectory: string read GetSystemDirectory stored False;
    property TempDirectory: string read GetTempDirectory stored False;
    property ProgramFilesDirectory: string read GetProgramFilesDirectory stored False;
    property ProgramFilesX86Directory: string read GetProgramFilesX86Directory stored False;
    property CommonFilesDirectory: string read GetCommonFilesDirectory stored False;

    property DesktopDirectory: string read GetDesktopDirectory stored False;
    property DocumentsDirectory: string read GetDocumentsDirectory stored False;
    property MusicDirectory: string read GetMusicDirectory stored False;
    property PicturesDirectory: string read GetPicturesDirectory stored False;
    property VideosDirectory: string read GetVideosDirectory stored False;
    property DownloadsDirectory: string read GetDownloadsDirectory stored False;
    property StartMenuDirectory: string read GetStartMenuDirectory stored False;
    property StartupDirectory: string read GetStartupDirectory stored False;

    property AppDataDirectory: string read GetAppDataDirectory stored False;
    property LocalAppDataDirectory: string read GetLocalAppDataDirectory stored False;
    property CommonAppDataDirectory: string read GetCommonAppDataDirectory stored False;

    property UserName: string read GetUserName stored False;
    property UserDirectory: string read GetUserDirectory stored False;
    property ComputerName: string read GetComputerName stored False;
    property UserDomain: string read GetUserDomain stored False;

    property WindowsVersion: string read GetWindowsVersion stored False;
    property WindowsBuild: string read GetWindowsBuild stored False;
    property WindowsProductName: string read GetWindowsProductName stored False;
    property SystemUptime: Int64 read GetSystemUptime stored False;

    property IsAdmin: Boolean read GetIsAdmin stored False;
    property Is64BitOS: Boolean read GetIs64BitOS stored False;
    property Is64BitProcess: Boolean read GetIs64BitProcess stored False;
  end;

implementation

uses
  Winapi.ActiveX, System.Win.ComObj;

{ TDiskList }

constructor TDiskList.Create;
begin
  inherited;
  FList := TList.Create;
  Refresh;
end;

destructor TDiskList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TDiskList.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    TDiskInfo(FList[I]).Free;
  FList.Clear;
end;

function TDiskList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDiskList.GetItem(Index: Integer): TDiskInfo;
begin
  Result := TDiskInfo(FList[Index]);
end;

procedure TDiskList.Refresh;
var
  Drives: DWORD;
  Drive: Char;
  DiskInfo: TDiskInfo;
  VolumeName: array[0..MAX_PATH] of Char;
  FileSystemName: array[0..MAX_PATH] of Char;
  SerialNumber, MaxComponentLength, FileSystemFlags: DWORD;
  RootPath: string;
  SectorsPerCluster, BytesPerSector, NumberOfFreeClusters, TotalNumberOfClusters: DWORD;
  DriveType: UINT;
begin
  Clear;
  Drives := GetLogicalDrives;

  for Drive := 'A' to 'Z' do
  begin
    if (Drives and 1) = 1 then
    begin
      RootPath := Drive + ':\';
      DriveType := GetDriveType(PChar(RootPath));

      if DriveType <> DRIVE_NO_ROOT_DIR then
      begin
        DiskInfo := TDiskInfo.Create;
        DiskInfo.FDriveLetter := Drive;

        case DriveType of
          DRIVE_REMOVABLE: DiskInfo.FDriveType := 'Removable';
          DRIVE_FIXED: DiskInfo.FDriveType := 'Fixed';
          DRIVE_REMOTE: DiskInfo.FDriveType := 'Network';
          DRIVE_CDROM: DiskInfo.FDriveType := 'CD-ROM';
          DRIVE_RAMDISK: DiskInfo.FDriveType := 'RAM Disk';
        else
          DiskInfo.FDriveType := 'Unknown';
        end;

        if GetVolumeInformation(PChar(RootPath), VolumeName, SizeOf(VolumeName),
          @SerialNumber, MaxComponentLength, FileSystemFlags,
          FileSystemName, SizeOf(FileSystemName)) then
        begin
          DiskInfo.FVolumeLabel := VolumeName;
          DiskInfo.FFileSystem := FileSystemName;
          DiskInfo.FSerialNumber := SerialNumber;
        end;

        if GetDiskFreeSpace(PChar(RootPath), SectorsPerCluster, BytesPerSector,
          NumberOfFreeClusters, TotalNumberOfClusters) then
        begin
          DiskInfo.FTotalSpace := Int64(TotalNumberOfClusters) * SectorsPerCluster * BytesPerSector;
          DiskInfo.FFreeSpace := Int64(NumberOfFreeClusters) * SectorsPerCluster * BytesPerSector;
        end;

        FList.Add(DiskInfo);
      end;
    end;
    Drives := Drives shr 1;
  end;
end;

{ TSysInfo }

constructor TSysInfo.Create(AOwner: TComponent);
begin
  inherited;
  FDiskList := TDiskList.Create;
end;

destructor TSysInfo.Destroy;
begin
  FDiskList.Free;
  inherited;
end;

procedure TSysInfo.RefreshAll;
begin
  FDiskList.Refresh;
end;

function TSysInfo.GetCPUName: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0') then
    begin
      if Reg.ValueExists('ProcessorNameString') then
        Result := Trim(Reg.ReadString('ProcessorNameString'));
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetCPUVendor: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0') then
    begin
      if Reg.ValueExists('VendorIdentifier') then
        Result := Reg.ReadString('VendorIdentifier');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetCPUIdentifier: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0') then
    begin
      if Reg.ValueExists('Identifier') then
        Result := Reg.ReadString('Identifier');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetCPUSpeed: Integer;
var
  Reg: TRegistry;
begin
  Result := 0;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\CentralProcessor\0') then
    begin
      if Reg.ValueExists('~MHz') then
        Result := Reg.ReadInteger('~MHz');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetCPUCount: Integer;
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;

function TSysInfo.GetCPUArchitecture: string;
var
  SysInfo: TSystemInfo;
begin
  GetNativeSystemInfo(SysInfo);
  case SysInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL: Result := 'x86 (32-bit)';
    PROCESSOR_ARCHITECTURE_AMD64: Result := 'x64 (64-bit)';
    PROCESSOR_ARCHITECTURE_ARM: Result := 'ARM';
    12: Result := 'ARM64';  // PROCESSOR_ARCHITECTURE_ARM64 = 12
    PROCESSOR_ARCHITECTURE_IA64: Result := 'IA64';
  else
    Result := 'Unknown';
  end;
end;

function TSysInfo.GetTotalMemory: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result := MemStatus.ullTotalPhys;
end;

function TSysInfo.GetAvailableMemory: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result := MemStatus.ullAvailPhys;
end;

function TSysInfo.GetMemoryLoad: Integer;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result := MemStatus.dwMemoryLoad;
end;

function TSysInfo.GetPageFileTotal: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result := MemStatus.ullTotalPageFile;
end;

function TSysInfo.GetPageFileFree: Int64;
var
  MemStatus: TMemoryStatusEx;
begin
  MemStatus.dwLength := SizeOf(MemStatus);
  GlobalMemoryStatusEx(MemStatus);
  Result := MemStatus.ullAvailPageFile;
end;

function TSysInfo.GetMainboardManufacturer: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\BIOS') then
    begin
      if Reg.ValueExists('SystemManufacturer') then
        Result := Reg.ReadString('SystemManufacturer');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetMainboardProduct: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\BIOS') then
    begin
      if Reg.ValueExists('SystemProductName') then
        Result := Reg.ReadString('SystemProductName');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetBIOSVendor: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\BIOS') then
    begin
      if Reg.ValueExists('BIOSVendor') then
        Result := Reg.ReadString('BIOSVendor');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetBIOSVersion: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\BIOS') then
    begin
      if Reg.ValueExists('BIOSVersion') then
        Result := Reg.ReadString('BIOSVersion');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetBIOSDate: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('HARDWARE\DESCRIPTION\System\BIOS') then
    begin
      if Reg.ValueExists('BIOSReleaseDate') then
        Result := Reg.ReadString('BIOSReleaseDate');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetWindowsDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Winapi.Windows.GetWindowsDirectory(Buffer, MAX_PATH);
  Result := Buffer;
end;

function TSysInfo.GetSystemDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  Winapi.Windows.GetSystemDirectory(Buffer, MAX_PATH);
  Result := Buffer;
end;

function TSysInfo.GetTempDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buffer);
  Result := Buffer;
end;

function TSysInfo.GetProgramFilesDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_PROGRAM_FILES, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetProgramFilesX86Directory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_PROGRAM_FILESX86, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetCommonFilesDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_PROGRAM_FILES_COMMON, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetDesktopDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_DESKTOPDIRECTORY, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetDocumentsDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_MYDOCUMENTS, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetMusicDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_MYMUSIC, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetPicturesDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_MYPICTURES, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetVideosDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_MYVIDEO, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetDownloadsDirectory: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders') then
    begin
      if Reg.ValueExists('{374DE290-123F-4565-9164-39C4925E467B}') then
        Result := Reg.ReadString('{374DE290-123F-4565-9164-39C4925E467B}');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetStartMenuDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_STARTMENU, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetStartupDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_STARTUP, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetAppDataDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetLocalAppDataDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetCommonAppDataDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_COMMON_APPDATA, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetUserName: string;
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
begin
  Size := SizeOf(Buffer);
  if Winapi.Windows.GetUserName(Buffer, Size) then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetUserDirectory: string;
var
  Buffer: array[0..MAX_PATH] of Char;
begin
  if SHGetFolderPath(0, CSIDL_PROFILE, 0, 0, Buffer) = S_OK then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetComputerName: string;
var
  Buffer: array[0..MAX_COMPUTERNAME_LENGTH] of Char;
  Size: DWORD;
begin
  Size := SizeOf(Buffer);
  if Winapi.Windows.GetComputerName(Buffer, Size) then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetUserDomain: string;
var
  Buffer: array[0..255] of Char;
  Size: DWORD;
begin
  Size := SizeOf(Buffer);
  if GetEnvironmentVariable('USERDOMAIN', Buffer, Size) > 0 then
    Result := Buffer
  else
    Result := '';
end;

function TSysInfo.GetWindowsVersion: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      if Reg.ValueExists('CurrentVersion') then
        Result := Reg.ReadString('CurrentVersion');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetWindowsBuild: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      if Reg.ValueExists('CurrentBuild') then
        Result := Reg.ReadString('CurrentBuild');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetWindowsProductName: string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly('SOFTWARE\Microsoft\Windows NT\CurrentVersion') then
    begin
      if Reg.ValueExists('ProductName') then
        Result := Reg.ReadString('ProductName');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TSysInfo.GetSystemUptime: Int64;
begin
  Result := GetTickCount64;
end;

function TSysInfo.GetIsAdmin: Boolean;
var
  TokenHandle: THandle;
  Elevation: TOKEN_ELEVATION;
  ReturnLength: DWORD;
begin
  Result := False;
  if OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, TokenHandle) then
  try
    if GetTokenInformation(TokenHandle, TokenElevation, @Elevation, SizeOf(Elevation), ReturnLength) then
      Result := Elevation.TokenIsElevated <> 0;
  finally
    CloseHandle(TokenHandle);
  end;
end;

function TSysInfo.GetIs64BitOS: Boolean;
type
  TIsWow64Process = function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
var
  IsWow64Process: TIsWow64Process;
  IsWow64: BOOL;
begin
  {$IFDEF WIN64}
  Result := True;
  {$ELSE}
  Result := False;
  IsWow64Process := GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');
  if Assigned(IsWow64Process) then
  begin
    if IsWow64Process(GetCurrentProcess, IsWow64) then
      Result := IsWow64;
  end;
  {$ENDIF}
end;

function TSysInfo.GetIs64BitProcess: Boolean;
begin
  {$IFDEF WIN64}
  Result := True;
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

end.
