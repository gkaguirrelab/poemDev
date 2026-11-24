function [SummaryTable, Stats] = lightTouchCorrelation(Results, T, Dx, options)
% Examines pseudo-VLSQ-8 light sensitivity, allodynia, and headache burden across POEM v3.1 groups.
%
% Syntax:
%   [SummaryTable, Stats] = lightTouchCorrelation(Results, T, Dx)
%   [SummaryTable, Stats] = lightTouchCorrelation(Results, T, Dx, figSaveDir="~/Desktop", saveFigs=true)
%
% Inputs:
%   Results, T, Dx     - Output tables from poemAnalysis_v3_1.
%   options.figSaveDir - Directory for saved PDF figures (default: "~/Desktop").
%   options.nBoots     - Bootstrap iterations for histograms (default: 100).
%   options.nBins      - Number of histogram bins (default: 7).
%   options.saveFigs   - Save figures as PDFs (default: true).
%
% Outputs:
%   SummaryTable       - Formatted patient summary table (MwoA vs. MwA).
%   Stats              - Struct containing correlations, t-tests, and aura comparisons.

arguments
    Results table
    T table
    Dx table
    options.figSaveDir (1,1) string = "~/Desktop"
    options.nBoots (1,1) double {mustBePositive, mustBeInteger} = 100
    options.nBins (1,1) double {mustBePositive, mustBeInteger} = 7
    options.saveFigs (1,1) logical = true
end

%% 1. Define Diagnostic Masks
woIdx       = Results.DxFull == "migraine w/out aura";
wIdx        = Results.DxFull == "migraine w/aura";
migraineIdx = woIdx | wIdx;
haFIdx      = Results.DxFull == "no headache";

varsToPlot   = {'MIDAS', 'PseudoVLSQ8', 'AllodyniaScore'};
scoreLabels  = {'MIDAS', 'pseudo-VLSQ-8', 'allodynia'};
xlimMax      = [400, 40, 25];
xtickSpacing = [100, 10, 5];

%% 2. Group Symptom Histograms (HAF vs. Migraine)
fig1 = makeFigure(6, 2);
tiledlayout(1, 3, 'TileSpacing', 'tight', 'Padding', 'tight');

for vv = 1:3
    vals = double(Results.(varsToPlot{vv}));
    [~, edges] = discretize(vals, options.nBins);
    nexttile(vv);

    if vv == 1
        % Skip HAF for MIDAS
        h = plotBootHist(vals, edges, {migraineIdx}, {'r'}, {[0.75 0.5 0.75]}, options.nBoots);
    else
        h = plotBootHist(vals, edges, {haFIdx, migraineIdx}, {'k', 'r'}, {[0.5 0.5 0.5], [0.75 0.5 0.75]}, options.nBoots);
    end
    styleHistAxes(vv, varsToPlot{vv}, scoreLabels{vv}, xlimMax(vv), xtickSpacing(vv));
    if vv == 2
        lg = legend(h, {sprintf('HAF, n=%d', sum(haFIdx)), sprintf('migr, n=%d', sum(migraineIdx))}, 'Location', 'north');
        lg.Box = 'off';
    end
end
saveIfRequested(fig1, options, "POEM_v3.1_GroupSymptomHistograms.pdf");

%% 3. Conditional Symptom Histograms (MwoA vs. MwA)
fig2 = makeFigure(6, 2);
tiledlayout(1, 3, 'TileSpacing', 'tight', 'Padding', 'tight');

for vv = 1:3
    vals = double(Results.(varsToPlot{vv}));
    [~, edges] = discretize(vals(migraineIdx), options.nBins);
    nexttile(vv);

    h = plotBootHist(vals, edges, {woIdx, wIdx}, {'r', 'b'}, {[0.75 0.5 0.5], [0.5 0.5 0.75]}, options.nBoots);
    [~, pKS, ksStat] = kstest2(vals(woIdx), vals(wIdx));
    styleHistAxes(vv, varsToPlot{vv}, scoreLabels{vv}, xlimMax(vv), xtickSpacing(vv));
    text(xlimMax(vv) * 0.05, 0.70, sprintf('KS = %.2f, p = %.1e', ksStat, pKS), 'HorizontalAlignment', 'left');

    if vv == 2
        lg = legend(h, {sprintf('w/out, n=%d', sum(woIdx)), sprintf('w/aura, n=%d', sum(wIdx))}, 'Location', 'north');
        lg.Box = 'off';
    end
end
saveIfRequested(fig2, options, "POEM_v3.1_ConditionalSymptomHistograms.pdf");

%% 4. Correlations & Light/Touch Scatter (MwoA vs. MwA)
[Stats.rhoMidasAllo, Stats.pMidasAllo]   = corr(Results.MIDAS(migraineIdx), Results.AllodyniaScore(migraineIdx), 'Type', 'Spearman', 'Rows', 'complete');
[Stats.rhoMidasLight, Stats.pMidasLight] = corr(Results.MIDAS(migraineIdx), Results.PseudoVLSQ8(migraineIdx), 'Type', 'Spearman', 'Rows', 'complete');
fprintf('Correlation of MIDAS with ASC-12 in migraine: Spearman rho = %.2f, p = %.1e\n', Stats.rhoMidasAllo, Stats.pMidasAllo);
fprintf('Correlation of MIDAS with pseudo-VLSQ-8 in migraine: Spearman rho = %.2f, p = %.1e\n', Stats.rhoMidasLight, Stats.pMidasLight);

fig3 = makeFigure(6, 3);
tiledlayout(1, 2, 'TileSpacing', 'tight', 'Padding', 'tight');

nexttile();
rWoA = plotScatterFit(Results.PseudoVLSQ8(woIdx), Results.AllodyniaScore(woIdx), 'r', 'M wo/Aura');
nexttile();
rWA  = plotScatterFit(Results.PseudoVLSQ8(wIdx),  Results.AllodyniaScore(wIdx),  'b', 'M w/Aura');
saveIfRequested(fig3, options, "POEM_v3.1_SymptomScatterPlot.pdf");

Stats.rLightTouch_woA   = rWoA;
Stats.rLightTouch_wA    = rWA;
Stats.pDiffLightTouch_r = compareCorrelationCoefficients(rWoA, rWA, sum(woIdx), sum(wIdx));
fprintf('Difference in correlation of pseudo-VLSQ-8 and touch sens between MwA and MwoA: p = %.4f\n', Stats.pDiffLightTouch_r);

%% 5. US Map of Respondents
fig4 = makeFigure(4, 3);
lat = double(T.LocationLatitude);
lon = double(T.LocationLongitude);
geoplot(lat(wIdx),  lon(wIdx),  'o', 'MarkerSize', 5, 'MarkerEdgeColor', 'none', 'MarkerFaceColor', [0 0 1]);
hold on
geoplot(lat(woIdx), lon(woIdx), 'o', 'MarkerSize', 5, 'MarkerEdgeColor', 'none', 'MarkerFaceColor', [1 0 0]);
geobasemap grayland
geolimits([25 50], [-130 -60]);
ax = gca();
ax.AxisColor = 'none';
ax.Grid = 'off';
ax.Scalebar.Visible = 'off';
saveIfRequested(fig4, options, "POEM_v3.1_MapAllRespondents.pdf");

%% 6. Group Comparisons (MwoA vs. MwA)
compVars   = {'MigraineFrequency', 'HAseverity', 'MIDAS'};
compLabels = {'Migraine Freq', 'Headache Severity', 'MIDAS'};
for i = 1:numel(compVars)
    v1 = Results.(compVars{i})(woIdx);
    v2 = Results.(compVars{i})(wIdx);
    [~, p, ~, st] = ttest2(v1, v2);
    Stats.ttest.(compVars{i}) = struct('mean_woA', mean(v1, 'omitnan'), 'mean_wA', mean(v2, 'omitnan'), ...
                                       'tstat', st.tstat, 'df', st.df, 'p', p);
    fprintf('%s woA = %.1f, wA = %.1f, t(%d df) = %.2f, p = %.2f\n', ...
            compLabels{i}, mean(v1, 'omitnan'), mean(v2, 'omitnan'), st.df, st.tstat, p);
end

%% 7. Patient Summary Table (MwoA vs. MwA)
idxList = {woIdx, wIdx};
rowNames = ["n", "SAAB-%F", "Age", "MIDAS", "MigraineFrequency", ...
            "HAseverity", "PseudoVLSQ8", "AllodyniaScore", "Chronic (%)", ...
            "Visual aura only (%)", "Sensory aura only (%)", "Visual + Sensory (%)"];
colData = strings(numel(rowNames), 2);

for g = 1:2
    idx = idxList{g};
    saab = Results.SAAB(idx);
    age  = Results.Age(idx);
    age  = age(age < 120); % Exclude implausible age entry (e.g., 558)

    colData(1, g) = string(sum(idx));
    colData(2, g) = sprintf('%.1f%%', 100 * sum(saab == "Female") / sum(saab == "Female" | saab == "Male"));
    colData(3, g) = fmtMeanSD(age);
    colData(4, g) = fmtMeanSD(Results.MIDAS(idx));
    colData(5, g) = fmtMeanSD(Results.MigraineFrequency(idx));
    colData(6, g) = fmtMeanSD(Results.HAseverity(idx));
    colData(7, g) = fmtMeanSD(Results.PseudoVLSQ8(idx));
    colData(8, g) = fmtMeanSD(Results.AllodyniaScore(idx));
    colData(9, g) = sprintf('%.1f%%', 100 * mean(Dx.Chronic(idx) == 1));
end

% Aura breakdown (MwA only)
colData(10:12, 1) = "-";
colData(10, 2) = sprintf('%.1f%%', 100 * sum(Dx.VisualAura(wIdx) == 1 & Dx.MultiAura(wIdx) == 0) / sum(wIdx));
colData(11, 2) = sprintf('%.1f%%', 100 * sum(Dx.SensAura(wIdx) == 1   & Dx.MultiAura(wIdx) == 0) / sum(wIdx));
colData(12, 2) = sprintf('%.1f%%', 100 * sum(Dx.VisualAura(wIdx) == 1 & Dx.SensAura(wIdx) == 1)  / sum(wIdx));

SummaryTable = table(colData(:, 1), colData(:, 2), 'VariableNames', {'MwoA', 'MwA'}, 'RowNames', rowNames);

%% 8. Light & Touch Sensitivity by Aura Subtype (within MwA)
auraLabels = {'visual', 'sens', 'vis+sens', 'other'};
visOnly    = wIdx & Dx.VisualAura == 1 & Dx.MultiAura == 0;
sensOnly   = wIdx & Dx.SensAura == 1   & Dx.MultiAura == 0;
visAndSens = wIdx & Dx.VisualAura == 1 & Dx.SensAura == 1;
otherAura  = wIdx & ~(visOnly | sensOnly | visAndSens);
auraMasks  = {visOnly, sensOnly, visAndSens, otherAura};

fprintf('\nPseudo-VLSQ-8 and touch sensitivity by aura subtype (within MwA):\n');
lightStore = cell(1, 4);
touchStore = cell(1, 4);
for i = 1:4
    lightStore{i} = Results.PseudoVLSQ8(auraMasks{i});
    touchStore{i} = Results.AllodyniaScore(auraMasks{i});
    fprintf('  %-8s (n=%3d): VLSQ-8 %.2f ± %.2f, touch %.2f ± %.2f\n', ...
            auraLabels{i}, sum(auraMasks{i}), ...
            mean(lightStore{i}, 'omitnan'), std(lightStore{i}, 'omitnan'), ...
            mean(touchStore{i}, 'omitnan'), std(touchStore{i}, 'omitnan'));
end

fprintf('\nPairwise aura subtype t-tests (p-values):\n');
for a = 1:3
    for b = (a + 1):4
        [~, pLight] = ttest2(lightStore{a}, lightStore{b});
        [~, pTouch] = ttest2(touchStore{a}, touchStore{b});
        fprintf('  %s vs %s: VLSQ-8 p = %.3f, touch p = %.3f\n', auraLabels{a}, auraLabels{b}, pLight, pTouch);
    end
end

end % main function

%% Local Helper Functions
function fig = makeFigure(wInches, hInches)
    fig = figure('Color', 'none', 'Units', 'inches', 'Position', [1, 1, wInches, hInches]);
    fig.PaperUnits = 'inches';
    fig.PaperSize = [wInches, hInches];
    fig.PaperPosition = [0, 0, wInches, hInches];
end

function saveIfRequested(fig, options, fileName)
    if options.saveFigs
        exportgraphics(fig, fullfile(options.figSaveDir, fileName), 'ContentType', 'vector', 'BackgroundColor', 'none');
    end
end

function handles = plotBootHist(vals, edges, maskCell, patchColors, lineColors, nBoots)
    X = edges(1:end-1) + diff(edges) / 2;
    plot([0, edges(end)], [0, 0], ':k');
    hold on
    handles = gobjects(1, numel(maskCell));
    for g = 1:numel(maskCell)
        subVals = vals(maskCell{g});
        subVals = subVals(~isnan(subVals));
        if isempty(subVals), continue; end

        Yboot = zeros(nBoots, numel(X));
        n = numel(subVals);
        for b = 1:nBoots
            Y = histcounts(subVals(randi(n, n, 1)), edges);
            Yboot(b, :) = Y ./ sum(Y);
        end
        Ymean = mean(Yboot, 1);
        Ysem  = std(Yboot, 0, 1);
        patch([X, fliplr(X)], [Ymean + Ysem, fliplr(Ymean - Ysem)], patchColors{g}, 'FaceAlpha', 0.1, 'EdgeColor', 'none');
        handles(g) = plot(X, Ymean, '.-', 'Color', lineColors{g}, 'MarkerSize', 10, 'LineWidth', 1.5);
    end
end

function styleHistAxes(tileIdx, titleStr, xLabelStr, xMax, xStep)
    ax = gca();
    ax.Color = 'none';
    box off
    ylim([-0.05, 0.75]);
    xlim([0, xMax]);
    ax.TickDir = 'out';
    ax.XTick = 0:xStep:xMax;
    ax.XTickLabelRotation = 45;
    if tileIdx == 1
        ylabel('Proportion subjects');
        ax.YTick = [0, 0.75];
    else
        ax.YTick = [];
    end
    title(titleStr);
    xlabel(xLabelStr);
    axis square
end

function r = plotScatterFit(x, y, faceColor, titlePrefix)
    scatter(x, y, 50, 'MarkerFaceColor', faceColor, 'MarkerEdgeColor', 'none', 'MarkerFaceAlpha', 0.25);
    hold on
    mdl = fitlm(x, y);
    xSort = sort(x);
    [ypred, yci] = predict(mdl, xSort, 'Prediction', 'curve');
    plot(xSort, ypred, 'k-', 'LineWidth', 2);
    plot(xSort, yci, 'k--', 'LineWidth', 1.5);

    r = corr(x, y, 'Rows', 'complete');
    axis square
    box off
    xlabel('Pseudo-VLSQ-8 score');
    ylabel('Allodynia score');
    ax = gca();
    ax.TickDir = 'out';
    ax.XTick = 10:10:40;
    xlim([5, 42]);
    ylim([-2.5, 25]);
    ax.FontSize = 14;
    title(sprintf('%s r = %.2f', titlePrefix, r));
end

function s = fmtMeanSD(x)
    s = sprintf('%.1f ± %.1f', mean(x, 'omitnan'), std(x, 'omitnan'));
end

function p = compareCorrelationCoefficients(r1, r2, n1, n2)
    z1 = 0.5 * log((1 + r1) / (1 - r1));
    z2 = 0.5 * log((1 + r2) / (1 - r2));
    z  = (z1 - z2) / sqrt(1 / (n1 - 3) + 1 / (n2 - 3));
    p  = 2 * (1 - normcdf(abs(z), 0, 1));
end