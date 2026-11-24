function [Results, T, Dx] = poemAnalysis_v3_1(spreadSheetName)
% Processes, classifies, and summarizes POEM v3.1 (SV_cub6KY8wuvPSBUi) data.
%
% Syntax:
%   [Results, T, Dx] = poemAnalysis_v3_1(spreadSheetName)
%
% Inputs:
%   spreadSheetName - Path to the Qualtrics POEM v3.1 CSV file.
%
% Outputs:
%   Results         - Summary table of demographics, diagnoses, and scores.
%   T               - Full pre-processed data table.
%   Dx              - ICHD-3 diagnostic classification table.

arguments
    spreadSheetName (1,1) string = "POEM_v3.1_April 11, 2024_12.18.csv"
end

%% 1. Load, Rename, and Filter Table
% Skip Qualtrics metadata rows 2 and 3 so numeric columns import cleanly
opts = detectImportOptions(spreadSheetName, 'VariableNamingRule', 'preserve');
opts.DataLines = [4 Inf];
T = readtable(spreadSheetName, opts);

T.Properties.VariableNames = getPoemV31ColumnNames();

% Drop completely empty rows/columns and invalid/preview responses
T = rmmissing(T, 'MinNumMissing', width(T));
T = rmmissing(T, 2, 'MinNumMissing', height(T));

isValid = ~strcmpi(string(T.Finished), "FALSE") & ...
          ~contains(string(T.AnsCorrect), "No") & ...
          ~contains(string(T.Status), "Survey Preview");
T = T(isValid, :);

T = convertvars(T, {'UserLanguage', 'RaceEth', 'GenderID', 'SAAB', 'SO'}, 'categorical');

% Inline matching helpers
isYes   = @(col) double(string(T.(col)) == "Yes");
hasText = @(col, pat) double(contains(string(T.(col)), pat));

%% 2. ICHD-3 Migraine & Aura Criteria
% Combine >4 hr (MigCritC/D) and <4 hr (MigCritC2/D2) symptom columns
migCritC_all = string(T.MigCritC) + " " + string(T.MigCritC2);
migCritD_all = string(T.MigCritD) + " " + string(T.MigCritD2);

T.migICHD_A   = isYes('MigCritA');
T.migICHD_B   = isYes('MigCritB');
T.migICHD_C1  = double(contains(migCritC_all, 'worse on one side'));
T.migICHD_C2  = double(contains(migCritC_all, 'pounding, pulsating, or throbbing'));
T.migICHD_C3  = double(contains(migCritC_all, 'moderate or severe in intensity'));
T.migICHD_C4  = double(contains(migCritC_all, 'made worse by routine activities'));
T.migICHD_C   = T.migICHD_C1 + T.migICHD_C2 + T.migICHD_C3 + T.migICHD_C4;
T.migICHD_Dp  = double(contains(migCritD_all, 'Sensitivity to light') & ...
                       contains(migCritD_all, 'Sensitivity to sound'));
T.migICHD_Dnv = double(contains(migCritD_all, 'Nausea and/or vomiting'));
T.migICHD_D   = double(T.migICHD_Dp | T.migICHD_Dnv);

% Aura criteria
auraHA = ["Before the headache", "During the headache"];
T.visAuraICHD_A    = isYes('VisAura2orMore');
T.sensAuraICHD_A   = isYes('SensAura2orMore');
T.speechAuraICHD_A = double(isYes('SpeechAura2orMore') & hasText('SpeechAura', ["Trouble saying words", "Unable to speak"]));

T.visAuraICHD_C1    = isYes('VisAuraSpread');
T.sensAuraICHD_C1   = isYes('SensAuraSpread');
T.AuraICHD_C2       = hasText('MultiSxAura_sametime', 'No');
T.visAuraICHD_C3    = double(hasText('VisAuraDur', '5 min to 1 hour')    | isYes('VisAuraDur_yn'));
T.sensAuraICHD_C3   = double(hasText('SensAuraDur', '5 min to 1 hour')   | isYes('SensAuraDur_yn'));
T.speechAuraICHD_C3 = double(hasText('SpeechAuraDur', '5 min to 1 hour') | isYes('SpeechAuraDur_yn'));
T.visAuraICHD_C4    = isYes('VisAuraUni');
T.sensAuraICHD_C4   = isYes('SensAuraUni');
T.speechAuraICHD_C4 = hasText('SpeechAura', 'Unable to speak');
T.visAuraICHD_C5    = hasText('VisAura', ["Zigzag lines", "Spots", "Stars", "Flashing lights", "Lines", "Heat waves"]);
T.sensAuraICHD_C5   = hasText('SensAura', 'Tingling');
T.visAuraICHD_C6    = hasText('VisAuraHArelation', auraHA);
T.sensAuraICHD_C6   = hasText('SensAuraHArelation', auraHA);
T.speechAuraICHD_C6 = hasText('SpeechAuraHArelation', auraHA);

T.visAuraICHD_C    = T.visAuraICHD_C1 + T.AuraICHD_C2 + T.visAuraICHD_C3 + T.visAuraICHD_C4 + T.visAuraICHD_C5 + T.visAuraICHD_C6;
T.sensAuraICHD_C   = T.sensAuraICHD_C1 + T.AuraICHD_C2 + T.sensAuraICHD_C3 + T.sensAuraICHD_C4 + T.sensAuraICHD_C5 + T.sensAuraICHD_C6;
T.speechAuraICHD_C = T.AuraICHD_C2 + T.speechAuraICHD_C3 + T.speechAuraICHD_C4 + T.speechAuraICHD_C6;

%% 3. Symptom, Burden, Allodynia, TheraSpecs, and Pseudo-VLSQ-8 Scores
% Pseudo-MIDAS
midasCols = {'HAmissedWork3mo', 'HAimpactWork3mo', 'HAmissedHousework3mo', 'HAimpactHousework3mo', 'HAmissedSocial3mo'};
midasKeys = {'1 - 3 days', '4 - 11 days', '12 - 24 days', '25 - 48 days', '49 - 72 days', '73 - 90 days'};
midasVals = [2, 7, 18, 36, 60, 81];
midasMat  = zeros(height(T), numel(midasCols));
for i = 1:numel(midasCols)
    midasMat(:, i) = mapContains(T.(midasCols{i}), midasKeys, midasVals, 0);
    T.("MIDAS" + i) = midasMat(:, i);
end
T.MIDAS_score = sum(midasMat, 2);

% Frequency & Continuous HA
T.MigraineFrequency = mapContains(T.MigFreq, {'0 - 4 days', '5 - 9 days', '10 - 15 days', '16 - 20 days', 'More than 20 days'}, 1:5, NaN);
T.ContinuousHA      = hasText('contHAsomeBreaks', 'no breaks');

% Allodynia (ASC-12, combining HA and headache-free blocks)
alloStems = {'Comb', 'Ponytail', 'Shave', 'EyeGlasses', 'Contacts', 'Earrings', ...
             'Necklace', 'TightClothes', 'Shower', 'Pillow', 'Heat', 'Cold'};
alloMat = zeros(height(T), numel(alloStems));
for i = 1:numel(alloStems)
    s1 = string(T.("Allodynia" + alloStems{i}));
    s2 = string(T.("Allodynia" + alloStems{i} + "_noHA"));
    score = double(contains(s1, 'Less than half the time') | contains(s2, 'Less than half the time'));
    score(contains(s1, 'Half the time or more') | contains(s2, 'Half the time or more')) = 2;
    alloMat(:, i) = score;
    T.("allodynia" + i) = score;
end
T.allodynia_score = sum(alloMat, 2);

% Associated & Visual Aura symptom indicators
T.photophobia  = double(contains(migCritD_all, 'Sensitivity to light'));
T.phonophobia  = double(contains(migCritD_all, 'Sensitivity to sound'));
T.nausea_vomit = double(contains(migCritD_all, 'Nausea'));

sxSpec = {
    'osmophobia',   'CAMS',    'Smell sensitivity';
    'lightheaded',  'CAMS',    'Light headedness';
    'blurryVision', 'CAMS',    'Blurry vision';
    'thinking',     'CAMS',    'Difficulty thinking';
    'neckPain',     'CAMS',    'Neck pain';
    'balance',      'CAMS',    'Balance problems';
    'spinning',     'CAMS',    'Room spinning';
    'ringing',      'CAMS',    'Ear ringing';
    'doubleVision', 'CAMS',    'Double vision';
    'visZigzag',    'VisAura', 'Zigzag lines';
    'visFlash',     'VisAura', 'Flashing lights';
    'visHeatWaves', 'VisAura', 'Heat waves';
    'visStars',     'VisAura', 'Stars';
    'visSpots',     'VisAura', 'Spots';
    'visLines',     'VisAura', 'Lines';
    'visLoss',      'VisAura', 'Vision loss'
};
for i = 1:size(sxSpec, 1)
    T.(sxSpec{i, 1}) = hasText(sxSpec{i, 2}, sxSpec{i, 3});
end

% TheraSpecs Photophobia Inventory (0 = Never ... 4 = Always)
tsMap = {
    'Light_HA', 'lightHA';               'Light_Dizzy', 'lightDizzy';
    'Light_EyeStrain', 'lightEyeStrain'; 'Light_BlurryVision', 'lightBlurryVision';
    'Light_LightIntolerant', 'lightIntolerant'; 'Light_Anxiety', 'lightAnxiety';
    'Fluorescent', 'lightFluorescent';   'Flicker', 'lightFlicker';
    'Headlight', 'lightHeadlight';       'OutdoorGlare', 'lightOutdoorGlare';
    'Screen', 'lightScreen';             'Sunlight', 'lightSunlight';
    'FlickeringTrees', 'lightTrees';     'SunglassesSunlight', 'lightGlassesSun';
    'SunglassesIndoors', 'lightGlassesInd'; 'SeekDarkness', 'lightSeekDark';
    'LightTvLimit', 'lightLimTV';        'LightDeviceLimit', 'lightLimDevice';
    'LightShopsLimit', 'lightLimShops';  'LightDriveLimit', 'lightLimDrive';
    'LightWorkLimit', 'lightLimWork';    'LightOutdoorLimit', 'lightLimOutdoor'
};
for i = 1:size(tsMap, 1)
    T.(tsMap{i, 2}) = mapContains(T.(tsMap{i, 1}), {'Rarely', 'Sometimes', 'Often', 'Always'}, 1:4, 0);
end
T.lightSx_score        = sum(T{:, {'lightHA','lightDizzy','lightEyeStrain','lightBlurryVision','lightIntolerant','lightAnxiety'}}, 2);
T.lightBother_score    = sum(T{:, {'lightFluorescent','lightFlicker','lightOutdoorGlare','lightTrees','lightSunlight','lightHeadlight','lightScreen'}}, 2);
T.lightAvoidance_score = sum(T{:, {'lightGlassesSun','lightGlassesInd','lightSeekDark','lightLimTV','lightLimDevice','lightLimShops','lightLimDrive','lightLimWork','lightLimOutdoor'}}, 2);
T.lightFlicker_score   = sum(T{:, {'lightFluorescent','lightFlicker','lightTrees','lightScreen'}}, 2);
T.lightStatic_score    = sum(T{:, {'lightOutdoorGlare','lightSunlight','lightHeadlight'}}, 2);

% Pseudo VLSQ-8 (Verriotto et al., 2017; each item coded 1 = Never ... 5 = Always, total 8-40)
T.vlsq1 = T.lightSunlight + 1;                          % Q1: Outdoors during daylight
T.vlsq2 = T.lightOutdoorGlare + 1;                      % Q2: Sense of glare
T.vlsq3 = T.lightFlicker + 1;                           % Q3: Flickering lights
T.vlsq4 = str2double(string(T.LightSensitivity));       % Q4: Worst severity in past 30 days (1-5)
T.vlsq5 = T.lightHA + 1;                                % Q5: Light-induced headache
T.vlsq6 = T.lightBlurryVision + 1;                      % Q6: Light-induced blurry vision
T.vlsq7 = max(T.lightLimTV, T.lightLimDevice) + 1;      % Q7: Limits TV or computer/device use
T.vlsq8 = T.lightGlassesInd + 1;                        % Q8: Dark glasses indoors
T.vlsq8_score = sum(T{:, {'vlsq1','vlsq2','vlsq3','vlsq4','vlsq5','vlsq6','vlsq7','vlsq8'}}, 2);

%% 4. ICHD-3 Diagnostic Classification
Dx = table(T.ZipCode, T.MigFreq, 'VariableNames', {'ZipCode', 'MigraineFrequency'});

critCount           = T.migICHD_A + T.migICHD_B + double(T.migICHD_C >= 2) + T.migICHD_D;
Dx.Migraine         = double(critCount == 4);
Dx.ProbableMigraine = double(critCount == 3);
Dx.Chronic          = hasText('MigFreq', ["16 - 20 days", "More than 20 days"]);

Dx.VisualAura = double(T.visAuraICHD_A == 1 & T.visAuraICHD_C >= 3);
Dx.SensAura   = double(T.sensAuraICHD_A == 1 & T.sensAuraICHD_C >= 3);
Dx.SpeechAura = double(T.speechAuraICHD_A == 1 & T.speechAuraICHD_C >= 3);
Dx.MultiAura  = double((Dx.VisualAura + Dx.SensAura + Dx.SpeechAura) >= 2);

hasAura = Dx.VisualAura | Dx.SensAura | Dx.SpeechAura;
noHA    = hasText('unprovokedHA_yn', 'No') | ...
          (hasText('HA_yn', 'No') & hasText('everHA', 'No') & hasText('everDiscomfort', 'No'));

dxCode  = NaN(height(T), 1);
dxCode(Dx.Migraine == 1 & ~hasAura)         = 1;
dxCode(Dx.Migraine == 1 &  hasAura)         = 2;
dxCode(Dx.ProbableMigraine == 1 & ~hasAura) = 3;
dxCode(Dx.ProbableMigraine == 1 &  hasAura) = 4;
dxCode(hasText('unprovokedHA_yn', 'Yes') & ~Dx.Migraine & ~Dx.ProbableMigraine) = 5;
dxCode(noHA) = 6;

dxLabels = {'migraine w/out aura', 'migraine w/aura', 'probable migraine w/out aura', ...
            'probable migraine w/aura', 'non-migraine headache', 'no headache'};
Dx.Dx  = categorical(dxCode, 1:6, dxLabels);
Dx.Dx2 = mergecats(Dx.Dx, dxLabels(1:4), 'migraine');

T.Dx       = Dx.Dx;
T.DxSimple = reordercats(mergecats(Dx.Dx2, dxLabels(5:6), 'not migraine'), {'not migraine', 'migraine'});

%% 5. Compile Summary Results Table
Results = table(categorical(T.ResponseID), 'VariableNames', {'ResponseID'});
Results.Age               = T.Age;
Results.ZipCode           = T.ZipCode;
Results.RaceEthnicity     = T.RaceEth;
Results.GenderID          = T.GenderID;
Results.SAAB              = T.SAAB;
Results.SO                = T.SO;
Results.HAageOnset        = categorical(T.HAageOnset);
Results.MigraineFamHx     = categorical(T.MigFamHx);
Results.DxFull            = T.Dx;
Results.DxSimple          = T.DxSimple;
Results.MIDAS             = T.MIDAS_score;
Results.MigraineFrequency = T.MigraineFrequency;
Results.HAseverity        = T.SeverityHA;
Results.ContinuousHA      = T.ContinuousHA;
Results.LightSensScore    = T.lightSx_score + T.lightBother_score + T.lightAvoidance_score - T.lightGlassesSun;
Results.PseudoVLSQ8       = T.vlsq8_score;
Results.AllodyniaScore    = T.allodynia_score;
Results.interAllodynia    = categorical(T.AllodyniaNoHA);
Results.PhoticSneeze      = categorical(string(T.LightSneeze));
Results.MotionSick        = categorical(T.MotionSick);

end % main function

%% Local Helper Functions
function out = mapContains(col, keys, vals, defaultVal)
    s = string(col);
    out = repmat(defaultVal, numel(s), 1);
    for k = 1:numel(keys)
        out(contains(s, keys{k})) = vals(k);
    end
end

function names = getPoemV31ColumnNames()
    names = {'StartDate','EndDate','Status','IPAddress','Progress','DurationToCompleteSec', ...
        'Finished','RecordedDate','ResponseID','LastName','FirstName','Email','ExternalReference','LocationLatitude', ...
        'LocationLongitude','DistributionChannel','UserLanguage','Q_RecaptchaScore','Age','ZipCode','RaceEth','AIAN','AIANtxt', ...
        'AIANtribe','AIANtribe_txt','Asian','Asian_txt','Black','Black_txt','Hispanic','Hispanic_txt','MENA','MENA_txt', ...
        'NHPI','NHPI_txt','White','White_txt','RaceEth_txt','GenderID','GenderID2','GenderID_txt','SAAB','SAAB_txt','SO', ...
        'SO2','SO_txt','HA_yn','unprovokedHA_yn','MigCritB','MigCritC','MigCritD','MigCritC2','MigCritD2','MigCritA', ...
        'MigFreq','MigRecent','VisAura','SensAura','SpeechAura','MultiSxAura_sametime','VisAura2orMore','VisAuraHArelation', ...
        'VisAuraRecent','VisAuraDur','VisAuraUni','VisAuraSpread','VisAuraDur_yn','SensAura2orMore','SensAuraHArelation', ...
        'SensAuraRecent','SensAuraDur','SensAuraUni','SensAuraSpread','SensAuraDur_yn','SpeechAura2orMore','SpeechAuraHArelation', ...
        'SpeechAuraHArecent','SpeechAuraDur','SpeechAuraDur_yn','menstrualMig','CAMS','contHA','contHAsomeBreaks','HAmissedWork3mo', ...
        'HAimpactWork3mo','HAmissedHousework3mo','HAimpactHousework3mo','HAmissedSocial3mo','HAdays3mo','SeverityHA', ...
        'HAageOnset','MotionSick','MigFamHx','LightSneeze','Light_HA','Light_Dizzy','Light_EyeStrain','Light_BlurryVision', ...
        'Light_LightIntolerant','Light_Anxiety','Fluorescent','Flicker','Headlight','OutdoorGlare','Screen','Sunlight', ...
        'FlickeringTrees','LightSensitivity','SunglassesSunlight','SunglassesIndoors','SeekDarkness','LightTvLimit','LightDeviceLimit', ...
        'LightShopsLimit','LightDriveLimit','LightWorkLimit','LightOutdoorLimit','AllodyniaComb','AllodyniaPonytail','AllodyniaShave', ...
        'AllodyniaEyeGlasses','AllodyniaContacts','AllodyniaEarrings','AllodyniaNecklace','AllodyniaTightClothes','AllodyniaShower', ...
        'AllodyniaPillow','AllodyniaHeat','AllodyniaCold','AllodyniaNoHA','AnsCorrect','Research','AllodyniaComb_noHA','AllodyniaPonytail_noHA', ...
        'AllodyniaShave_noHA','AllodyniaEyeGlasses_noHA','AllodyniaContacts_noHA','AllodyniaEarrings_noHA','AllodyniaNecklace_noHA', ...
        'AllodyniaTightClothes_noHA','AllodyniaShower_noHA','AllodyniaPillow_noHA','AllodyniaHeat_noHA','AllodyniaCold_noHA', ...
        'everHA','everDiscomfort'};
end