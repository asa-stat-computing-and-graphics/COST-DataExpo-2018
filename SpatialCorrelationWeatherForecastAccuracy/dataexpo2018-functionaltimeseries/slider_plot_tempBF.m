function [] = slider_plot_tempBF(LonVec,LatVec,BFValues,plotflag)
% Plot different plots according to slider location.
if nargin <= 3
    plotflag = '';
end
nLon = length(LonVec);
nLat = length(LatVec);
LonVecExt = kron(LonVec,ones(nLat,1));
LatVecExt = kron(ones(nLon,1),LatVec);
nBF = size(BFValues,2);

LonMin = floor(min(LonVec));
LonMax = ceil(max(LonVec));
LatMin = floor(min(LatVec));
LatMax = ceil(max(LatVec));
ValueMin = floor(min(BFValues(:)));
ValueMax = ceil(max(BFValues(:)));

figure('units','pixels',...
    'position',[100 100 900 900],...
    'menubar','none',...
    'name','slider_plot',...
    'numbertitle','off',...
    'resize','off');
axes('unit','pix',...
    'position',[50 80 800 775]);
if strcmp(plotflag,'surf')
    LN = surf(reshape(LonVecExt,nLon,nLat),reshape(LatVecExt,nLon,nLat),reshape(BFValues(:,1),nLon,nLat));
else
    LN = scatter3(LonVecExt,LatVecExt,BFValues(:,1),9,BFValues(:,1));
end
colormap(jet)
xlabel('Longitude')
ylabel('Latitude')
zlabel('Forecast Error')
xlim([LonMin,LonMax])
ylim([LatMin,LatMax])
zlim([ValueMin,ValueMax])

title('Basis Function 1');
uicontrol('style','slide',...
    'unit','pix',...
    'position',[50 10 800 30],...
    'min',1,'max',nBF,'val',1,...
    'sliderstep',[1/(nBF-1) 10/(nBF-1)],...
    'callback',{@sl_call,BFValues,LN,nLon,nLat,plotflag});
rotate3d on

function [] = sl_call(varargin)
% Callback for the slider.
[h,S,LN,nLon,nLat,plotflag] = varargin{[1,3,4,5,6,7]};  % calling handle and data structure.

if strcmp(plotflag,'surf')
    set(LN,'zdata',reshape(S(:,round(get(h,'value'))),nLon,nLat))
else
    set(LN,'zdata',S(:,round(get(h,'value'))))
    set(LN,'cdata',S(:,round(get(h,'value'))))
end
title(sprintf('Basis Function %d',round(get(h,'value'))))
