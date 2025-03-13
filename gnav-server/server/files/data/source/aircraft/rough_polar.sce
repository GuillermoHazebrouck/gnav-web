// Rough polar for the 64_1_A212 NACA profile
//clear; 
P_rough=[-0.0805552798273576, 0.00938225854481686
          0.1186925336480309, 0.00918744002911689
          0.2931115743344836, 0.00938887602157297
          0.5474623814828292, 0.01063304988474561
          0.7422529567253782, 0.01296707805313833
          0.8572867610024741, 0.01510134665651986
          0.9324112982386481, 0.01698697459991838
          0.9724852669232775, 0.01802900660644762];

CL_rough=P_rough(:,1);
CD_rough=P_rough(:,2)*rf+ro;

scf(0);

// Find the rough polar polynomial
poly_rough=polyfit(CL_rough, CD_rough, 4);

mprintf('Rough     => %10.8f, %10.8f, %10.8f, %10.8f, %10.8f\n', poly_rough(5), poly_rough(4), poly_rough(3), poly_rough(2), poly_rough(1));

// Reconstruct the polar using the 4th degree polynomial using CL steps
CL_poly_rough= (min(CL_rough):0.005:1.4*max(CL_rough));
CD_poly_rough= poly_rough(1)*CL_poly_rough.^4 + poly_rough(2)*CL_poly_rough.^3 + poly_rough(3)*CL_poly_rough.^2 + poly_rough(4)*CL_poly_rough + poly_rough(5);
plot(CL_poly_rough, CD_poly_rough, 'c--');

// The second degree polynomial is too optimistic at high CL
//p2=polyfit(CL, CD, 2);
//disp(p2);

// Reconstruct the polar using the 2nd degree polynomial using CL steps
//CL_p2= (min(CL):0.005:1.3*max(CL));
//CD_p2= p2(1)*CL_p2.^2 + p2(2)*CL_p2 + p2(3)
//plot(CL_p2, CD_p2, 'b-');
