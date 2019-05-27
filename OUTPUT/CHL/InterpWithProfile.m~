
%Interpolation function from Morel and Maritorena (2001)

function[data] = InterpWithProfile(meas,prof)
    Interp = interp1(prof(:,1), prof(:,2), meas(:,1));
    delta = interp1(meas(:,1), Interp - meas(:,2), prof(:,1),'linear','extrap');
	data = [prof(:,1) prof(:,2) - delta];
end
