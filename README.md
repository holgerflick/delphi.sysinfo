# TSysInfo - Delphi System Information Component

A comprehensive Delphi VCL component that provides detailed information about computer hardware, operating system, and system directories using Windows API.

## Features

### Hardware Information
- **CPU Information**
  - CPU Name, Vendor, Identifier
  - CPU Speed (MHz)
  - Number of processors/cores
  - CPU Architecture (x86, x64, ARM, ARM64)

- **Memory Information**
  - Total physical memory
  - Available memory
  - Memory load percentage
  - Page file size (total and free)

- **Mainboard Information**
  - Manufacturer
  - Product name
  - BIOS Vendor, Version, and Date

- **Disk Information**
  - List of all drives (A-Z)
  - Drive letter, type (Fixed, Removable, Network, CD-ROM, RAM Disk)
  - Volume label and file system
  - Total and free space
  - Serial number

### System Directories
- Windows Directory
- System Directory
- Temp Directory
- Program Files Directory
- Program Files (x86) Directory
- Common Files Directory

### User Directories
- Desktop
- Documents
- Music
- Pictures
- Videos
- Downloads
- Start Menu
- Startup
- User Profile Directory

### Application Data Directories
- AppData (Roaming)
- Local AppData
- Common AppData (All Users)

### System Information
- Current user name
- Computer name
- User domain
- Windows version and build number
- Windows product name
- System uptime (in milliseconds)
- Is running as administrator
- Is 64-bit operating system
- Is 64-bit process

## Installation

### Method 1: Installing via IDE

1. Open Delphi IDE
2. Open the runtime package:
   - File → Open → Select `SysInfoR.dpk`
   - Right-click on the package → Compile
3. Open the design-time package:
   - File → Open → Select `SysInfoD.dpk`
   - Right-click on the package → Compile
   - Right-click on the package → Install
4. The component will appear in the "System" category of the Tool Palette

### Method 2: Manual Installation

1. Add the directory containing `SysInfo.pas` to your project's search path:
   - Project → Options → Delphi Compiler → Search path
   - Add the path to the directory

2. Add `SysInfo` to your uses clause:
   ```pascal
   uses
     SysInfo;
   ```

## Usage

### Design-Time Usage

1. Drop a `TSysInfo` component onto your form from the "System" category
2. Access properties at runtime:

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('CPU: ' + SysInfo1.CPUName);
  ShowMessage('Total Memory: ' + IntToStr(SysInfo1.TotalMemory div (1024*1024)) + ' MB');
  ShowMessage('User: ' + SysInfo1.UserName);
  ShowMessage('Documents: ' + SysInfo1.DocumentsDirectory);
end;
```

### Runtime Usage

```pascal
uses
  SysInfo;

procedure TForm1.Button1Click(Sender: TObject);
var
  SysInfo: TSysInfo;
  I: Integer;
begin
  SysInfo := TSysInfo.Create(nil);
  try
    // CPU Information
    Memo1.Lines.Add('CPU Name: ' + SysInfo.CPUName);
    Memo1.Lines.Add('CPU Vendor: ' + SysInfo.CPUVendor);
    Memo1.Lines.Add('CPU Speed: ' + IntToStr(SysInfo.CPUSpeed) + ' MHz');
    Memo1.Lines.Add('CPU Count: ' + IntToStr(SysInfo.CPUCount));
    Memo1.Lines.Add('CPU Architecture: ' + SysInfo.CPUArchitecture);
    Memo1.Lines.Add('');

    // Memory Information
    Memo1.Lines.Add('Total Memory: ' + IntToStr(SysInfo.TotalMemory div (1024*1024)) + ' MB');
    Memo1.Lines.Add('Available Memory: ' + IntToStr(SysInfo.AvailableMemory div (1024*1024)) + ' MB');
    Memo1.Lines.Add('Memory Load: ' + IntToStr(SysInfo.MemoryLoad) + '%');
    Memo1.Lines.Add('');

    // Mainboard Information
    Memo1.Lines.Add('Mainboard Manufacturer: ' + SysInfo.MainboardManufacturer);
    Memo1.Lines.Add('Mainboard Product: ' + SysInfo.MainboardProduct);
    Memo1.Lines.Add('BIOS Vendor: ' + SysInfo.BIOSVendor);
    Memo1.Lines.Add('BIOS Version: ' + SysInfo.BIOSVersion);
    Memo1.Lines.Add('BIOS Date: ' + SysInfo.BIOSDate);
    Memo1.Lines.Add('');

    // Disk Information
    Memo1.Lines.Add('Disks:');
    for I := 0 to SysInfo.DiskList.Count - 1 do
    begin
      with SysInfo.DiskList[I] do
      begin
        Memo1.Lines.Add(Format('  %s: %s (%s) - %s', [
          DriveLetter + ':\',
          VolumeLabel,
          FileSystem,
          DriveType
        ]));
        Memo1.Lines.Add(Format('    Total: %d MB, Free: %d MB', [
          TotalSpace div (1024*1024),
          FreeSpace div (1024*1024)
        ]));
      end;
    end;
    Memo1.Lines.Add('');

    // System Directories
    Memo1.Lines.Add('Windows Directory: ' + SysInfo.WindowsDirectory);
    Memo1.Lines.Add('System Directory: ' + SysInfo.SystemDirectory);
    Memo1.Lines.Add('Temp Directory: ' + SysInfo.TempDirectory);
    Memo1.Lines.Add('Program Files: ' + SysInfo.ProgramFilesDirectory);
    Memo1.Lines.Add('');

    // User Directories
    Memo1.Lines.Add('Desktop: ' + SysInfo.DesktopDirectory);
    Memo1.Lines.Add('Documents: ' + SysInfo.DocumentsDirectory);
    Memo1.Lines.Add('Music: ' + SysInfo.MusicDirectory);
    Memo1.Lines.Add('Pictures: ' + SysInfo.PicturesDirectory);
    Memo1.Lines.Add('Videos: ' + SysInfo.VideosDirectory);
    Memo1.Lines.Add('Downloads: ' + SysInfo.downloadsDirectory);
    Memo1.Lines.Add('');

    // AppData Directories
    Memo1.Lines.Add('AppData: ' + SysInfo.AppDataDirectory);
    Memo1.Lines.Add('Local AppData: ' + SysInfo.LocalAppDataDirectory);
    Memo1.Lines.Add('Common AppData: ' + SysInfo.CommonAppDataDirectory);
    Memo1.Lines.Add('');

    // User Information
    Memo1.Lines.Add('User Name: ' + SysInfo.UserName);
    Memo1.Lines.Add('Computer Name: ' + SysInfo.ComputerName);
    Memo1.Lines.Add('User Domain: ' + SysInfo.UserDomain);
    Memo1.Lines.Add('User Directory: ' + SysInfo.UserDirectory);
    Memo1.Lines.Add('');

    // Windows Information
    Memo1.Lines.Add('Windows: ' + SysInfo.WindowsProductName);
    Memo1.Lines.Add('Version: ' + SysInfo.WindowsVersion);
    Memo1.Lines.Add('Build: ' + SysInfo.WindowsBuild);
    Memo1.Lines.Add('');

    // System Status
    Memo1.Lines.Add('Uptime: ' + IntToStr(SysInfo.SystemUptime div 1000) + ' seconds');
    Memo1.Lines.Add('Is Administrator: ' + BoolToStr(SysInfo.IsAdmin, True));
    Memo1.Lines.Add('64-bit OS: ' + BoolToStr(SysInfo.Is64BitOS, True));
    Memo1.Lines.Add('64-bit Process: ' + BoolToStr(SysInfo.Is64BitProcess, True));
  finally
    SysInfo.Free;
  end;
end;
```

### Refreshing Disk Information

The disk list is automatically populated when the component is created. To refresh the disk information:

```pascal
SysInfo1.RefreshAll;
```

Or refresh just the disk list:

```pascal
SysInfo1.DiskList.Refresh;
```

## Properties Reference

### CPU Properties
- `CPUName: string` - Full CPU name (e.g., "Intel(R) Core(TM) i7-9700K CPU @ 3.60GHz")
- `CPUVendor: string` - CPU vendor (e.g., "GenuineIntel", "AuthenticAMD")
- `CPUIdentifier: string` - CPU identifier string
- `CPUSpeed: Integer` - CPU speed in MHz
- `CPUCount: Integer` - Number of logical processors
- `CPUArchitecture: string` - Architecture (x86, x64, ARM, ARM64, IA64)

### Memory Properties
- `TotalMemory: Int64` - Total physical memory in bytes
- `AvailableMemory: Int64` - Available physical memory in bytes
- `MemoryLoad: Integer` - Memory usage percentage (0-100)
- `PageFileTotal: Int64` - Total page file size in bytes
- `PageFileFree: Int64` - Available page file size in bytes

### Mainboard Properties
- `MainboardManufacturer: string` - Mainboard manufacturer
- `MainboardProduct: string` - Mainboard product name
- `BIOSVendor: string` - BIOS vendor
- `BIOSVersion: string` - BIOS version
- `BIOSDate: string` - BIOS release date

### Disk Properties
- `DiskList: TDiskList` - Collection of disk information
  - `Count: Integer` - Number of drives
  - `Items[Index]: TDiskInfo` - Access individual drive information
    - `DriveLetter: Char` - Drive letter (A-Z)
    - `VolumeLabel: string` - Volume label
    - `FileSystem: string` - File system type (NTFS, FAT32, etc.)
    - `DriveType: string` - Drive type (Fixed, Removable, Network, CD-ROM, RAM Disk)
    - `TotalSpace: Int64` - Total space in bytes
    - `FreeSpace: Int64` - Free space in bytes
    - `SerialNumber: Cardinal` - Volume serial number

### System Directory Properties
- `WindowsDirectory: string` - Windows installation directory
- `SystemDirectory: string` - Windows System32 directory
- `TempDirectory: string` - Temporary files directory
- `ProgramFilesDirectory: string` - Program Files directory
- `ProgramFilesX86Directory: string` - Program Files (x86) directory
- `CommonFilesDirectory: string` - Common Files directory

### User Directory Properties
- `DesktopDirectory: string` - User's Desktop folder
- `DocumentsDirectory: string` - User's Documents folder
- `MusicDirectory: string` - User's Music folder
- `PicturesDirectory: string` - User's Pictures folder
- `VideosDirectory: string` - User's Videos folder
- `DownloadsDirectory: string` - User's Downloads folder
- `StartMenuDirectory: string` - User's Start Menu folder
- `StartupDirectory: string` - User's Startup folder

### AppData Directory Properties
- `AppDataDirectory: string` - User's AppData\Roaming folder
- `LocalAppDataDirectory: string` - User's AppData\Local folder
- `CommonAppDataDirectory: string` - ProgramData (All Users) folder

### User Information Properties
- `UserName: string` - Current user's login name
- `ComputerName: string` - Computer name
- `UserDomain: string` - User's domain or workgroup
- `UserDirectory: string` - User's profile directory

### System Information Properties
- `WindowsVersion: string` - Windows version number
- `WindowsBuild: string` - Windows build number
- `WindowsProductName: string` - Windows edition name
- `SystemUptime: Int64` - System uptime in milliseconds
- `IsAdmin: Boolean` - True if running with administrator privileges
- `Is64BitOS: Boolean` - True if running on 64-bit Windows
- `Is64BitProcess: Boolean` - True if this is a 64-bit process

## Requirements

- Delphi XE2 or later (for 64-bit support)
- Windows XP or later
- VCL Framework

## License

This component is provided as-is for free use in both commercial and non-commercial projects.

## Notes

- All directory properties use Windows API calls (SHGetFolderPath, GetWindowsDirectory, etc.)
- Memory values are in bytes; divide by (1024*1024) to get MB or (1024*1024*1024) to get GB
- Some properties may return empty strings if the information is not available on the system
- Administrator check requires Windows Vista or later
- The component automatically initializes disk information when created
- Use `RefreshAll` method to update dynamic information (disk space, available memory, etc.)

## Example Project

See the usage examples above for a complete demonstration of all features.

## Support

For issues, questions, or contributions, please refer to the component source code and Windows API documentation.
