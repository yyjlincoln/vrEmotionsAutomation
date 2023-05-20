import re
import os
import subprocess

PROJECT_ROOT = "/Users/yyjlincoln/Downloads/IMU"

# Load template
with open("template.r.tpl", 'r', encoding='utf-8') as f:
    TEMPLATE = f.read()

# Parse template for variables
varsIncludingRepeating = re.findall(r'{{{(.*?)}}}', TEMPLATE)
vars = []

for v in varsIncludingRepeating:
    if v not in vars:
        vars.append(v)

# print("The template was parsed successfully and it requires the following \
# variables:")

# for v in vars:
#     print('-', v)

varMap = {v: None for v in vars}

assert 'subjectNumber' in varMap
assert 'sessionNumber' in varMap
assert 'numOfLogFilesLessOne' in varMap
assert 'numOfRuns' in varMap
assert 'xStartTimestamp' in varMap
assert 'PROJECT_ROOT' in varMap

varMap['PROJECT_ROOT'] = PROJECT_ROOT


def main(subjectNumber=None, sessionNumber=None):
    def getLogFunction():
        print("\n\n\n")
        f = open(
            f"vrEmotions_Subject_{subjectNumber}_Session_{sessionNumber}.log", 'w', encoding='utf-8')

        def log(*args, **kw):
            print(*args, **kw)
            print(*args, **kw, file=f)
        return log, lambda: f.close()

    log, clean = getLogFunction()

    log(f"Processing Subject {subjectNumber} Session {sessionNumber}.")

    def process(subjectNumber=None, sessionNumber=None):
        if subjectNumber is None:
            subjectNumber = int(input("Enter subject number: "))
        if sessionNumber is None:
            sessionNumber = int(input("Enter session number: "))
        log(
            f"Generating .R file for Subject{subjectNumber}_Session{sessionNumber}")
        varMap['subjectNumber'] = subjectNumber
        varMap['sessionNumber'] = sessionNumber

        log("Finding files...")
        SESSION_PATH = getSessionPath(subjectNumber, sessionNumber)
        METADATA_PATH = getMetadataPath()
        ensureExist(SESSION_PATH, METADATA_PATH, PROJECT_ROOT)
        log("Found root: ", PROJECT_ROOT)
        log("Found session: ", SESSION_PATH)
        log("Found metadata: ", METADATA_PATH)
        startTimestamp = readStartTimestamp(METADATA_PATH, subjectNumber, sessionNumber)
        log("Found start timestamp: ", startTimestamp)
        varMap['xStartTimestamp'] = startTimestamp
        numOfLogFiles = getNumberOfLogFilesAndRename(SESSION_PATH)
        log(f"Found {numOfLogFiles} log files.")
        varMap['numOfLogFilesLessOne'] = numOfLogFiles - 1
        numOfRuns = getNumberOfRunsAndRename(
            SESSION_PATH, subjectNumber, sessionNumber)
        log(f"Found {numOfRuns} runs.")
        varMap['numOfRuns'] = numOfRuns

        content = TEMPLATE
        for v in varMap:
            if v is None:
                raise Exception(f"Could not resolve all vars: {v}.")
            content = content.replace('{{{'+v+'}}}', str(varMap[v]))

        with open(f"vrEmotions_Subject_{subjectNumber}_Session_{sessionNumber}.r", 'w', encoding='utf-8') as f:
            f.write(content)

        log(
            f"Generated vrEmotions_Subject_{subjectNumber}_Session_{sessionNumber}.r")

        log("Running analysis...")
        result = executeCommand(
            f"Rscript vrEmotions_Subject_{subjectNumber}_Session_{sessionNumber}.r")

        if 'error' in result.lower():
            log("An error was thrown in the analysis.")
            log(result)
            raise Exception("An error was thrown in the analysis: " + result)
        log("Analysis completed successfully.")
        log(result)

    try:
        process(subjectNumber, sessionNumber)
    except Exception as e:
        log(f"Failed to process: {e}")
        raise
    finally:
        clean()


def ensureExist(*path: str):
    for p in path:
        if not os.path.exists(p):
            raise Exception(f"{p} does not exist")


def ensureCanFindProject() -> bool:
    if not os.path.exists(PROJECT_ROOT):
        return False
    if not os.path.isdir(PROJECT_ROOT):
        return False
    return True


def getSessionPath(subjectNumber: int, sessionNumber: int, prefix=None) -> str:
    if not prefix:
        return os.path.join(
            PROJECT_ROOT, f'Subject{subjectNumber}_Session{sessionNumber}')
    else:
        return os.path.join(
            PROJECT_ROOT, f'{prefix}_Subject{subjectNumber}_Session{sessionNumber}')


def getMetadataPath() -> str:
    return os.path.join(
        PROJECT_ROOT, 'metadata.csv')


def readStartTimestamp(metadataPath: str, subjectNumber: int, sessionNumber: int) -> int:
    raise Exception("TODO: Implement this.")
    with open(metadataPath, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        if len(lines) == 0:
            raise Exception("metadata.csv is empty")
        # Discard heading
        lines = lines[1:]
        for lineNum in range(len(lines)):
            line = lines[lineNum]
            if line.lower().startswith(f'subject{subjectNumber}_session{sessionNumber}'):
                # In R, the index starts with 1.
                # So we need to add one to this.
                return lineNum + 1
    raise Exception(
        f"Could not find offset for Subject{subjectNumber}_Session{sessionNumber}")


def getLogFilePathBySessionPath(sessionPath: str) -> str:
    return os.path.join(sessionPath, 'IMU_Right')


def getNumberOfLogFilesAndRename(sessionPath: str) -> int:
    logFilePath = getLogFilePathBySessionPath(sessionPath)
    logFiles = []
    for fileName in os.listdir(logFilePath):
        filePath = os.path.join(logFilePath, fileName)
        if fileName.lower().startswith('log'):
            if fileName.lower().endswith('.txt'):
                os.rename(filePath, filePath[:-4] + '.csv')
                fileName = fileName[:-4] + ".csv"
            logFiles.append(fileName)

    # logFiles = list(filter(lambda x: x, os.listdir(logFilePath)))
    print("Found log file:")
    for f in logFiles:
        print('-', f)
    return len(logFiles)


def getNumberOfRunsAndRename(sessionPath: str, subjectNum: int, sessionNum: int) -> int:
    DIR = sessionPath
    print("Found runs:")
    fileNum = 0
    if not os.path.isdir(DIR):
        raise Exception("Invalid directory")
    for file in os.listdir(DIR):
        if file.endswith(".csv"):
            nameSplit = file.split("_")
            newName = None
            if len(nameSplit) == 4 and nameSplit[3].startswith("Snippet"):
                newName = nameSplit[0] + "_" + \
                    nameSplit[1] + "_" + nameSplit[2] + ".csv"
                os.rename(os.path.join(DIR, file), os.path.join(DIR, newName))
                # print("Renamed " + file + " to " + newName)
            if file.lower().startswith(f"subject{subjectNum}_session{sessionNum}_run"):
                # Already renamed
                fileNum += 1
                print("-", file if newName is None else newName)
    return fileNum


def executeCommand(command, stdin=None):
    process = subprocess.Popen(
        command.split(' '), stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        stdin=subprocess.PIPE)
    if stdin:
        process.stdin.write(stdin.encode())
    process.stdin.close()
    output = ''
    while process.poll() is None:
        try:
            process.stdout.flush()
        except Exception:
            break
        output += process.stdout.readline().decode('utf-8')
    output += process.stdout.read().decode('utf-8')
    return output


hasEverProcessedAnyFile = False
for subject in range(1, 100):
    for session in range(1, 10):
        if os.path.exists(getSessionPath(subject, session)):
            hasEverProcessedAnyFile = True
            try:
                main(subjectNumber=subject, sessionNumber=session)
                os.rename(getSessionPath(subject, session), getSessionPath(
                    subject, session, prefix="Processed"))
            except Exception as e:
                os.rename(getSessionPath(subject, session), getSessionPath(
                    subject, session, prefix="Error"))
                print(e)

if not hasEverProcessedAnyFile:
    print("Could not find any file to process.")
    print("Ensure that your project root is correct: " + PROJECT_ROOT)