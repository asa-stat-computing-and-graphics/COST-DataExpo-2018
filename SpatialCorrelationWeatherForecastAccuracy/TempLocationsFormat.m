% Format strings for Temperature Slider Plot by City
load('DataExpo2018.mat', 'locations')
locationsFormatted = cell(111,1);
for i = 1:111
    locationsFormatted{i} = [locations.city{i},', ',locations.state{i},...
                             ' (Lon ',num2str(locations.longitude(i),4),...
                             ', Lat ',num2str(locations.latitude(i),4),')'];
end