#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#
# FUNCTION
#	CalcSh	
#
# FILE
#	CalcSh.f
#
# DESCRIPTION
#
# AUTHORS
#      Piet Ruardij
#
# CHANGE_LOG
#	Created at Thu Nov 06 04:50:12 PM CET 2003
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

function real CalcSh( real E, real diameter, real diffusion)


variable

	real Pe =0.0;

end


compute
	Pe= pow(0.5* diameter,2.0) * E /diffusion;

	if ( Pe < 0.01 )
		return ( 1.0 + 0.29 *  sqrt(Pe));
	end
	if ( Pe < 100.0 )
		return ( 1.014 + 0.51 *  sqrt(Pe));
	end
	return ( 0.55 * pow(Pe,0.3333));
end

end
