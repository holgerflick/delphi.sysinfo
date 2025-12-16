# Quick Installation Guide

## Installation Steps

### Option 1: Install as a Package (Recommended)

1. **Open Delphi IDE**

2. **Install Runtime Package**
   - File → Open Project
   - Navigate to the component directory
   - Open `SysInfoR.dpk`
   - Right-click on the project in Project Manager
   - Select "Compile"
   - Wait for compilation to complete

3. **Install Design-Time Package**
   - File → Open Project
   - Open `SysInfoD.dpk`
   - Right-click on the project in Project Manager
   - Select "Compile"
   - Right-click again and select "Install"
   - You should see a confirmation message
   - The component will appear in the "System" palette

4. **Verify Installation**
   - Open or create a VCL Forms application
   - Look for "TSysInfo" in the Tool Palette under "System" category
   - Drag and drop it onto your form

### Option 2: Add to Project Directly

1. **Add Unit to Project**
   - Open your Delphi project
   - Project → Add to Project
   - Select `SysInfo.pas`
   - Add `SysInfo` to your unit's uses clause

2. **Create Component at Runtime**
   ```pascal
   var
     SysInfo: TSysInfo;
   begin
     SysInfo := TSysInfo.Create(Self);
     try
       // Use the component
       ShowMessage(SysInfo.CPUName);
     finally
       SysInfo.Free;
     end;
   end;
   ```

## Testing the Installation

### Run the Demo Application

1. Open `Demo\SysInfoDemo.dproj`
2. Press F9 to compile and run
3. The demo will display all available system information
4. Click through the tabs to see different information categories

### Quick Test in Your Application

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  Info: TSysInfo;
begin
  Info := TSysInfo.Create(nil);
  try
    ShowMessage('CPU: ' + Info.CPUName + #13#10 +
                'Memory: ' + IntToStr(Info.TotalMemory div (1024*1024)) + ' MB' + #13#10 +
                'User: ' + Info.UserName);
  finally
    Info.Free;
  end;
end;
```

## Troubleshooting

### "Unit not found" error
- Make sure the component directory is in your Library Path
- Tools → Options → Language → Delphi → Library → Library Path
- Add the path to the directory containing `SysInfo.pas`

### "Package already installed" error
- Tools → Options → Packages
- Find and uninstall the old version first
- Then install the new version

### Component not visible in Tool Palette
- Component → Install Packages
- Make sure the package is checked
- If not, click "Add" and browse to the .bpl file
- Restart the IDE if necessary

### Compilation errors
- Make sure you're using Delphi XE2 or later
- Check that all required units are available (Winapi.Windows, System.Win.Registry, etc.)
- For older Delphi versions, you may need to adjust unit names (remove System. prefix)

## Uninstallation

1. Open Delphi IDE
2. Component → Install Packages
3. Select "System Information Component (Design-Time)"
4. Click "Remove"
5. Restart IDE

## Requirements

- Delphi XE2 or later (for full 64-bit support)
- Windows XP or later
- VCL Framework (not compatible with FMX)

## Next Steps

- Read the [README.md](README.md) for detailed property documentation
- Explore the [Demo](Demo/) application for usage examples
- Check out the source code in `SysInfo.pas` to understand implementation details

## Support

For issues or questions, refer to the source code comments and Windows API documentation.
