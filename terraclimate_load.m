cd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/summaries/");
ppt_info = ncinfo("TerraClimate19812010_ppt.nc");

ppt = ncread("TerraClimate19812010_ppt.nc", "ppt");
% default stored as lat, lon, time
%ppt(ppt == -2147483648) = NaN;
pptt = permute(ppt, [2, 1, 3]);
% transpose first two dimensions, now it's lon, lat, time (x, y, t)
% element selection is row, col, depth
imagesc(pptt(:,:,6))
plot(squeeze(pptt(1200, 2400, :)))


%cd("/Users/davidmiller/dlm_files/keenan_postdoc/terraclimate/summaries/");
pet_info = ncinfo("TerraClimate19812010_pet.nc");

ppt7 = ppt(:,:,7);
ppt7 = ppt7';
imagesc(ppt7)