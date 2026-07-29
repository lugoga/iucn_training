# IUCN Bahari Yetu: Git & GitHub Workflow Guide

Version control is a critical component of reproducible science. It allows you to keep track of modifications to your code, collaborate with your supervisors and peers, and submit your research scripts for monthly code reviews.

---

## Step 1: Initial Git Configuration

If you are using Git for the first time, you must introduce yourself to Git. Open your terminal (Git Bash on Windows or Terminal on macOS) and run:

```bash
# Configure your name (use your real name)
git config --global user.name "Your Name"

# Configure your email (must match your GitHub email account)
git config --global user.email "your.email@example.com"
```

---

## Step 2: Setting up Your Repository

During the bootcamp, you will publish your research analysis directory as a private or public repository on GitHub.

### Scenario A: Clone an Existing Repository
If you already have a repository online and want to copy it onto your local machine:
```bash
git clone https://github.com/your-username/my-thesis-analysis.git
```

### Scenario B: Initialize a New Local Repository
If you have a local directory that you want to upload:
1. Open RStudio/Positron and create a new project.
2. Initialize Git:
   ```bash
   git init
   ```
3. Add a `.gitignore` file to ensure you don't commit large datasets (which should stay local):
   ```bash
   # Add data/ folder to ignore list
   echo "data/" >> .gitignore
   echo ".Rproj.user" >> .gitignore
   ```

---

## Step 3: The Daily Coding Loop (Commit and Push)

As you write scripts and run models, follow this 3-step loop to back up your changes.

```
[Write/Edit Code] -> [git add] -> [git commit] -> [git push]
```

### 1. Stage Your Changes
Select which files you want to save.
```bash
# Stage a specific file
git add scripts/01_clean_data.R

# Stage all updated files (except ignored ones)
git add .
```

### 2. Commit Your Changes
Save a snapshot of the staged files with a short descriptive message.
```bash
git commit -m "Cleaned raw tree DBH measurements and added plot totals"
```

### 3. Push to GitHub
Send your local commits to your online repository.
```bash
git push origin main
```

---

## Step 4: Troubleshooting Common Issues

### Issue 1: Large Files Error
GitHub rejects files larger than 100MB. If you have large datasets, ensure they are in your `/data/` folder and that `data/` is included in your `.gitignore` file.

### Issue 2: Remote Changes (Always Pull First!)
If you or your mentor edited your code on another computer or directly on GitHub, your local copy will be out of sync. Always download updates before pushing new code:
```bash
git pull origin main
```
If conflicts occur, RStudio will mark the lines with `<<<<<<<` and `>>>>>>>`. Open the file, choose the correct lines, delete the conflict markers, save, commit, and push.
