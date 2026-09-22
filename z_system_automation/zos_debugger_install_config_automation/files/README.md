# Required Installation Files

This directory should contain the IBM z/OS Debugger installation files required for the automation process.

## ⚠️ Important Notice

**The installation files are NOT included in this repository.** You must download them separately and place them in this directory before running the automation scripts.

## Required Files

You need to download and place the following files in this directory:

### 1. Base Product RELFILES
- **Filename:** `HADRXXX.pax.Z`
- **Description:** IBM z/OS Debugger base product installation package (RELFILES)
- **Required for:** SMP/E installation (Step 2)

### 2. Latest PTF Package
- **Filename:** `IBM.HADRXXX.PTF####` (or latest available PTF)
- **Description:** Program Temporary Fix (PTF) package for the debugger
- **Required for:** PTF installation (Step 3)

## Download Instructions

1. **Obtain the files** from one of the following sources:
   - IBM Shopz (https://www.ibm.com/software/shopzseries/)
   - IBM Fix Central (https://www.ibm.com/support/fixcentral/)
   - Your IBM representative or support contact

2. **Download both files** to your local machine
   - ⚠️ **IMPORTANT:** Download files in **BINARY mode** to prevent corruption
   - If using FTP/SFTP, ensure binary transfer mode is enabled
   - Do not download as text or ASCII mode

3. **Place the files** in this `files/` directory:
   ```
   Debug-Config-Automation/
   └── files/
       ├── README.md (this file)
       ├── HADRXXX.pax.Z
       └── IBM.HADRXXX.PTF####
   ```

4. **Verify the files** are in place:
   ```bash
   ls -lh files/
   ```

   You should see both files listed with their respective sizes.

## Update Configuration

After placing the files in this directory, you **MUST** update the `variables.yml` file in the root directory to match your file names:

### Edit `variables.yml`:

```yaml
# --- PTF Installation Variables ---
ptf_file: IBM.HADRXXX.PTF####    # Update this if your PTF filename is different
SMP_file: HADRXXX.pax.Z          # Update this if your RELFILES filename is different
```

**Important:** If you download a newer PTF version, update the `ptf_file` variable to match the exact filename.

## File Naming Convention

- **RELFILES:** Typically named `HADRXXX.pax.Z` (where HADRH00 is the FMID)
- **PTF Files:** Named in format `IBM.HADRH00.UOxxxxx` (where xxxxx is the PTF number)

## Verification Checklist

Before running the automation, verify:

- [ ] Both required files are present in the `files/` directory
- [ ] File names in `variables.yml` match the actual file names
- [ ] Files are not corrupted (check file sizes match expected values)
- [ ] You have read access to both files


## Troubleshooting

### Files Not Found Error
If you encounter "file not found" errors during automation:
1. Verify files exist in the `files/` directory
2. Check file names match exactly (case-sensitive)
3. Ensure `variables.yml` has correct file names
4. Verify file permissions allow read access

### Wrong PTF Version
If you need to use a different PTF version:
1. Download the correct PTF file
2. Place it in the `files/` directory
3. Update `ptf_file` in `variables.yml` with the new filename
4. Remove or backup the old PTF file

## Security Note

**Do not commit these files to version control.** The `.gitignore` file is configured to exclude these installation files from the repository.

## Support

For questions about:
- **Obtaining files:** Contact your IBM representative or IBM Support
- **File compatibility:** Refer to IBM z/OS Debugger documentation
- **Automation issues:** See the main [README.md](../README.md) and [User Guide](../doc/User_Guide.md)

---

**Last Updated:** June 2026