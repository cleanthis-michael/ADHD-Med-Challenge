###################################################################################################
###################################################################################################
###################################################################################################
###############   ADHD Med Challenge / Task / Go_No_Go task / GNG task_JRC / data   ###############
###############   The following code summarizes the MedChallenge data into a csv    ###############
##############   with 1 row per participant, reporting RT and accuracy data per Run   #############
###################################################################################################
###################################################################################################
###################################################################################################


from os import listdir
from scipy.stats import norm


# The following function will access the name of the participant's file to identify the task condition
# i.e., 'GNG Go Only', 'GNG Regular', or 'GNG Reward' (when taskOnly is True)
# or identify the task condition and the day on which the task was completed
# i.e., '1_GNGregular', '2_GNGregular', '3_GNGregular' (when taskOnly is False)

def FindTaskCondition(participantFile, taskOnly):
    characterIndex = 0  # as the following loop iterates across each character of the file name, track that character's index
    underscoreCount = 0  # the task condition is written between the second and third underscore in the file name

    for character in participantFile:
        characterIndex += 1
        if character == '_':
            underscoreCount += 1
            if taskOnly == True:
                if underscoreCount == 2: startFrom = characterIndex  # get the index of the second underscore
            else:
                if underscoreCount == 1: startFrom = characterIndex  # get the index of the first underscore

            if underscoreCount == 3: endTo = characterIndex - 1  # get the index of the third underscore

    taskCondition = participantFile[startFrom: endTo]
    return taskCondition

allParticipantData = listdir()  # read the directory that contains each participant's data

resultsCSV = open('ADHD MedChallenge Results Summary.csv', 'w')  # create the csv file that will summarize each participant's data (1 row per participant)
allParticipantData = sorted(allParticipantData)  # sort the files in alphabetical order
filesUsedCSV = open('ADHD MedChallenge Files Used.csv', 'w')  # create the csv file that will summarize the files used for each participant
subjectRTcsv = open('ADHD MedChallenge Response Times.csv', 'w')  # create the csv file that will summarize each participant's RTs

# Write the following in the first line of the csv files, which describes what each column will represent
resultsCsvColumnLabels = "SubjectID,Sex,Age,Group,Session_Order,Run1_GoOnly_Mean,Run1_GoOnly_SD,Run1_GoOnly_Omission,Run1_GoOnly_InvEff," \
                         "Run1_GNGReg_Mean,Run1_GNGReg_SD,Run1_GNGReg_Omission,Run1_GNGReg_Commission,Run1_GNGReg_dprim,Run1_GNGReg_InvEff," \
                         "Run1_GNGRew_Mean,Run1_GNGRew_SD,Run1_GNGRew_Omission,Run1_GNGRew_Commission,Run1_GNGRew_dprim,Run1_GNGRew_InvEff," \
                         "Run2_GNGReg_Mean,Run2_GNGReg_SD,Run2_GNGReg_Omission,Run2_GNGReg_Commission,Run2_GNGReg_dprim,Run2_GNGReg_InvEff," \
                         "Run2_GNGRew_Mean,Run2_GNGRew_SD,Run2_GNGRew_Omission,Run2_GNGRew_Commission,Run2_GNGRew_dprim,Run2_GNGRew_InvEff," \
                         "Run3_GNGReg_Mean,Run3_GNGReg_SD,Run3_GNGReg_Omission,Run3_GNGReg_Commission,Run3_GNGReg_dprim,Run3_GNGReg_InvEff," \
                         "Run3_GNGRew_Mean,Run3_GNGRew_SD,Run3_GNGRew_Omission,Run3_GNGRew_Commission,Run3_GNGRew_dprim,Run3_GNGRew_InvEff"

resultsCSV.write(resultsCsvColumnLabels)

filesUsedCsvColumnLabels = 'SubjectID,Run1_GoOnly,Run1_GNGReg,Run1_GNGRew,Run2_GNGReg,Run2_GNGRew,Run3_GNGReg,Run3_GNGRew'
filesUsedCSV.write(filesUsedCsvColumnLabels)

subjectRTcsvColumnLabels = 'SubjectID,Group,Task,Session'
subjectRTcsv.write(subjectRTcsvColumnLabels)
numberOfPossibleRTs = 188  # in the GNGreg/GNGrew tasks, there are 188 'Go' trials
for rtIndex in range(1, (numberOfPossibleRTs + 1)):
    subjectRTcsv.write(',RT-{0}'.format(rtIndex))
#subjectRTcsv.write(',Number_of_Responses')
#subjectRTcsv_NoR_Index = 192

# Create a list that contains all of the subject IDs whose data will be summarized in resultsCSV
listOfSubjects = ['1005','1006','1010','1011','1012','1013','1015','1016','1019','1020','1022','1023','1024','1025',
                  '1026','1027','1028','1029','1031','1034','1035','1036','1037','1038','1040','1042','1044','1045',
                  '1046','1048','1049','1050','1053','1054','1055','1056','1057','1060','1062','1064','1065','1067',
                  '1068','1069','1070','1071','1076','1077','1081', '1083', '1085']

tasks = ['1_GNGgoonly','1_GNGregular','1_GNGreward','2_GNGregular','2_GNGreward','3_GNGregular','3_GNGreward']
exceptionFiles = ['1013','1025', '1050','1056','1062','1081', '1083']
# exceptionFiles is a variable that contains subjectIDs with at least 1 'problematic' file

# Set up variables to access each participant's demographic information
with open('Demographics Form.csv', 'r') as file:
    participantDemographics = file.readlines()  # store each participant's demographics in a list of strings

# The following converts 'participantDemographics' from a list of strings to a list of sub-lists
# Each sub-list will now contain 1 participant's demographic data
for entry in range(len(participantDemographics)):
    editedData = participantDemographics[entry].strip('\n')
    listedData = editedData.split(',')
    participantDemographics[entry] = listedData

# Save the relevant indices into variables that will be used to get each participant's demographic data
subjectIDindex = participantDemographics[0].index('Subject ID')
sexIndex = participantDemographics[0].index('Child sex (M/F)')
ageIndex = participantDemographics[0].index('Child age')
groupIndex = participantDemographics[0].index('Group')

# Create a loop that will access each participant's data in each iteration, extract the relevant data, and update the csv files
for subject in listOfSubjects:
    fileList = []  # will contain only the files needed for each participant (e.g., no 'practice' files, only 'csv' files)
    for d, data in enumerate(allParticipantData):  # d is an index, 'data' is a file
        if (allParticipantData[d].find(subject) == 0) and (allParticipantData[d].find('practice') == -1) and (allParticipantData[d].find('.csv') > -1):
            fileList.append(allParticipantData[d])
    fileList = sorted(fileList)
    print(fileList)

    ### Get each participant's demographic data ###
    subjectIndex = 0
    demographicsListIndex = 0
    while subjectIndex == 0:  # access only the specific participant's list
        for sublist in participantDemographics:
            if sublist[0] == subject: subjectIndex = demographicsListIndex
            demographicsListIndex += 1

    subjectAge = participantDemographics[subjectIndex][ageIndex]
    subjectSex = participantDemographics[subjectIndex][sexIndex]
    subjectGroup = participantDemographics[subjectIndex][groupIndex]
    resultsCSV.write('\n{0},{1},{2},{3},,'.format(subject,subjectSex,subjectAge,subjectGroup))
    filesUsedCSV.write('\n{0},'.format(subject))

    ## Create new lists that will contain each subject's files and will be used to write in the results summary file ##
    codedCompletedTasks = []  # 0 = no completion (no file), 1 = completion, 2 = unwanted file
    checkLongestLength = (len(tasks) >= len(fileList))  # if false, there are more files than tasks
                                                        # if true, either all tasks completed or some were not completed

    if subject not in exceptionFiles:
        if checkLongestLength:
            for task in tasks:
                if any(task in participantFile for participantFile in fileList): codedCompletedTasks.append('1')
                else: codedCompletedTasks.append('0')

        else:
            for file in fileList:
                dayAndTaskCondition = FindTaskCondition(file, False)
                if dayAndTaskCondition in tasks: codedCompletedTasks.append('1')
                else: codedCompletedTasks.append('2')
    print(codedCompletedTasks)

    fileListIndex = 0
    newFileList = []  # will contain '0' if the participant did not complete each task,
                      # the name of the file if they did, and '2' for problematic files

    if subject not in exceptionFiles:
        for codedTask in codedCompletedTasks:
            if codedTask == '0': newFileList.append('0')
            elif codedTask == '2':
                newFileList.append('2')
                fileListIndex += 1
            else:
                newFileList.append(fileList[fileListIndex])
                fileListIndex += 1

    else:  # hard-code for participants with problematic files
        if subject == '1013': newFileList = ['1013_1_GNGgoonly_2017_Aug_05_1425.csv','1013_1_GNGregular_2017_Aug_05_1431_complete (created by CM).csv',
                                             '1013_1_GNGreward_2017_Aug_05_1447.csv','1013_2_GNGregular_2017_Aug_24_0939.csv',
                                             '1013_2_GNGreward_2017_Aug_24_1005.csv','1013_3_GNGregular_2017_Sep_09_0943.csv','0']

        elif subject == '1025': newFileList = ['1025_1_GNGgoonly_2018_May_19_1348.csv','1025_1_GNGregular_2018_May_19_1354.csv',
                                               '1025_1_GNGreward_2018_May_19_1410_complete (created by CM).csv','1025_2_GNGregular_2018_Jun_09_1039.csv',
                                               '1025_2_GNGreward_2018_Jun_09_1055.csv','1025_3_GNGregular_2018_Jun_14_1033.csv',
                                               '1025_3_GNGreward_2018_Jun_14_1048.csv']

        elif subject == '1050': newFileList = ['0','0','0','1050_2_GNGregular_2019_Jan_27_1341.csv','1050_2_GNGreward_2019_Jan_27_1357.csv',
                                               '1050_3_GNGregular_2019_Feb_03_1056.csv','1050_3_GNGreward_2019_Feb_03_1112.csv']

        elif subject == '1056': newFileList = ['0','1056_1_GNGregular_2019_Mar_09_1336.csv','1056_1_GNGreward_2019_Mar_09_1400.csv',
                                               '1056_2_GNGregular_2019_Mar_23_1248.csv','1056_2_GNGreward_2019_Mar_23_1305.csv',
                                               '1056_3_GNGregular_2019_Apr_14_0932.csv','0']

        elif subject == '1062': newFileList = ['1062_1_GNGgoonly_2019_Mar_21_1423.csv','1062_1_GNGregular_2019_Mar_21_1430.csv',
                                               '1062_1_GNGreward_2019_Mar_21_1446.csv','1062_2_GNGregular_2019_Apr_14_1301.csv',
                                               '1062_2_GNGreward_2019_Apr_14_1318.csv','1062_3_GNGregular_2019_Apr_24_1238_complete (created by CM).csv',
                                               '1062_3_GNGreward_2019_Apr_24_1302_complete (created by CM).csv']

        elif subject == '1081': newFileList = ['1081_1_GNGgoonly_2019_Aug_21_1148.csv','1081_1_GNGregular_2019_Aug_21_1154.csv',
                                               '1081_1_GNGreward_2019_Aug_21_1209.csv','1081_2_GNGregular_2019_Aug_25_1119.csv',
                                               '1081_2_GNGreward_2019_Aug_25_1134.csv','1081_3_GNGregular_2019_Sep_08_1114.csv',
                                               '1081_3_GNGreward_2019_Sep_08_1131_complete (created by CM).csv']

        elif subject == '1083': newFileList = ['1083_1_GNGgoonly_2019_Sep_25_1326.csv', '1083_1_GNGregular_2019_Sep_25_1347 (created by CM).csv',
                                               '1083_1_GNGreward_2019_Sep_25_1354.csv', '1083_2_GNGregular_2019_Sep_26_0846.csv',
                                               '1083_2_GNGreward_2019_Sep_26_0901.csv', '1083_3_GNGregular_2019_Oct_06_0954.csv',
                                               '1083_3_GNGreward_2019_Oct_06_1010.csv']


    print(newFileList)

    ### Loop through the list that now contains all the relevant files and codes ###
    newFileListIndex = 0
    for participantFile in newFileList:

        if (participantFile != '0') and (participantFile != '2'):  # i.e., if the participant completed a task

            dayAndTaskCondition = FindTaskCondition(participantFile, False)
            if dayAndTaskCondition in tasks:
                taskCondition = FindTaskCondition(participantFile, True)  # GNGgoonly, GNGregular, GNGreward
                with open(participantFile, 'r') as file:
                    participantData = file.readlines()  # store each participant's data in a list of strings

                # The following converts 'participantData' from a list of strings to a list of sub-lists
                # Each sub-list will now contain 1 list for each row of the accessed files
                for entry in range(len(participantData)):
                    editedData = participantData[entry].strip('\n')
                    listedData = editedData.split(',')
                    participantData[entry] = listedData

                try:
                    correctResponseIndex = participantData[0].index('trial_response.corr')
                    trialNumberIndex = participantData[0].index('trial')
                    trialTypeIndex = participantData[0].index('trialtype')
                    rtIndex = participantData[0].index('trial_response.rt')

                    # Set up the variables that are needed to subsequently compute RT and accuracy data
                    numberOfTrials = 0
                    reactionTimes = []
                    trialType = []  # will be used to find number of 'Go' and 'NoGo' trials
                    responses = []  # '0' is an incorrect response, '1' is a correct response
                    numberOfOmissionErrors = 0
                    numberOfCommissionErrors = 0

                    # The following loop accesses each trial's data
                    # Index 0 has the column headers, so start from index 1 for each trial
                    for trial in participantData[1:]:

                        if trial[trialNumberIndex] != '':  # between runs, there is one row without data => this is not needed
                            trialType.append(trial[trialTypeIndex])
                            responses.append(trial[correctResponseIndex])
                            numberOfTrials += 1

                            # Only access RT data in 'Go' trials ('NoGo' trials have no response hence no RT) in which the subject
                            # correctly responded (i.e., did not make an omission error) and so the RT will not be blank
                            if (trial[trialTypeIndex] == 'Go') and (trial[rtIndex] != ''):
                                trialRT = trial[rtIndex]
                                reactionTimes.append(float(trialRT))

                            if trial[correctResponseIndex] == '0':  # i.e., when the participant makes an error:
                                if trial[trialTypeIndex] == 'Go': numberOfOmissionErrors += 1
                                else: numberOfCommissionErrors += 1

                    ### Compute the data that will be saved in the csv file ###
                    # Mean Response Time
                    meanRT = sum(reactionTimes) / len(reactionTimes)

                    # Standard Deviation of Response Time
                    squaredDifferences = []
                    for RT in reactionTimes:
                        squaredDifference = (RT - meanRT) ** 2
                        squaredDifferences.append(squaredDifference)
                    meanSquaredDifferences = sum(squaredDifferences) / (len(squaredDifferences) - 1)
                    sdRT = (meanSquaredDifferences ** 0.5)
                    # sdRT2 = std(reactionTimes, ddof=1)

                    ### Accuracy Data ###
                    numberOfGoTrials = trialType.count('Go')
                    proportionOfOmissionErrors = numberOfOmissionErrors / numberOfGoTrials
                    hitRate = (1 - proportionOfOmissionErrors)

                    numberOfCorrectResponses = 0
                    for response in responses:
                        if response == '1': numberOfCorrectResponses += 1
                    percentageCorrectResponses = (numberOfCorrectResponses / len(responses)) * 100
                    inverseEfficiency = (meanRT * 1000) / percentageCorrectResponses

                    ### Write the relevant data in the corresponding csv files ###
                    if taskCondition == 'GNGgoonly': resultsCSV.write('{0},{1},{2},{3},'.format(meanRT,sdRT,proportionOfOmissionErrors,inverseEfficiency))

                    else:  # there are no 'NoGo' trials in the 'Go Only' task
                        numberOfNoGoTrials = trialType.count('NoGo')
                        proportionOfCommissionErrors = numberOfCommissionErrors / numberOfNoGoTrials
                        zHitRate = norm.ppf(hitRate)
                        zFalseAlarm = norm.ppf(proportionOfCommissionErrors)
                        d_prime = zHitRate - zFalseAlarm
                        resultsCSV.write('{0},{1},{2},{3},{4},{5},'.format(meanRT,sdRT,proportionOfOmissionErrors,proportionOfCommissionErrors,d_prime,inverseEfficiency))

                    filesUsedCSV.write('{0},'.format(participantFile))

                    stringReactionTimes = ''
                    # numberOfResponses = str(len(reactionTimes))
                    for RT in reactionTimes: stringReactionTimes += ',{0}'.format(str(RT))
                    participantSession = participantFile[5]
                    subjectRTcsv.write('\n{0},{1},{2},{3}{4}'.format(subject,subjectGroup,taskCondition,
                                       participantSession,stringReactionTimes))


                except ValueError:  # for empty files, simply skip through them
                    print(participantFile + ': ValueError')
                    continue

        elif participantFile == '2': continue  # if the file is problematic, skip through it

        else:  # if the participant did not complete the task
            filesUsedCSV.write('NA,')
            if newFileListIndex == 0: resultsCSV.write('NA,NA,NA,NA,')  # index 0 = GoOnly task, which only has 4 entries
            else: resultsCSV.write('NA,NA,NA,NA,NA,NA,')  # the rest of the tasks have 6 entries

        newFileListIndex += 1

resultsCSV.close()
filesUsedCSV.close()
subjectRTcsv.close()