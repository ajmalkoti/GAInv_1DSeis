# ogpf libray
# Rev. 0.22 of March 9th, 2018
# Licence: MIT

# gnuplot global setting
set term wxt size 640,480 enhanced font "verdana,10" title "ogpf libray: Rev. 0.22 of March 9th, 2018"

# ogpf extra configuration
# -------------------------------------------
# color definitions
set style line 1 lc rgb "#800000" lt 1 lw 2
set style line 2 lc rgb "#ff0000" lt 1 lw 2
set style line 3 lc rgb "#ff4500" lt 1 lw 2
set style line 4 lc rgb "#ffa500" lt 1 lw 2
set style line 5 lc rgb "#006400" lt 1 lw 2
set style line 6 lc rgb "#0000ff" lt 1 lw 2
set style line 7 lc rgb "#9400d3" lt 1 lw 2

# Axes
set border linewidth 1.15
set tics nomirror

# grid
# Add light grid to plot
set style line 102 lc rgb "#d6d7d9" lt 0 lw 1
set grid back ls 102

# plot style
set style data linespoints

# -------------------------------------------

 
# plot scale
 
# Annotation: title and labels
set title "Synthetic seismogram"
set xlabel "Time"
set ylabel "Amplitude"
 
# axes setting

plot "-" notitle with linespoints lt 2 pt 4
   0.00000000      -0.00000000    
   1.00000005E-03  -0.00000000    
   2.00000009E-03  -0.00000000    
   3.00000003E-03  -0.00000000    
   4.00000019E-03  -0.00000000    
   5.00000035E-03  -0.00000000    
   6.00000005E-03  -0.00000000    
   7.00000022E-03  -0.00000000    
   8.00000038E-03  -0.00000000    
   9.00000054E-03  -9.36175472E-18
   1.00000007E-02  -1.64010593E-16
   1.10000009E-02  -1.49729008E-15
   1.20000001E-02  -9.47710606E-15
   1.30000003E-02  -4.67033232E-14
   1.40000004E-02  -1.90851593E-13
   1.50000006E-02  -6.72856844E-13
   1.60000008E-02  -2.10313429E-12
   1.70000009E-02  -5.94594079E-12
   1.80000011E-02  -1.54417347E-11
   1.90000013E-02  -3.72992644E-11
   2.00000014E-02  -8.46733933E-11
   2.10000016E-02  -1.82271531E-10
   2.20000017E-02  -3.75001058E-10
   2.30000019E-02  -7.42568618E-10
   2.40000002E-02  -1.42416545E-09
   2.50000004E-02  -2.66027556E-09
   2.60000005E-02  -4.86341323E-09
   2.70000007E-02  -8.73727579E-09
   2.80000009E-02  -1.54759796E-08
   2.90000010E-02  -2.70941776E-08
   3.00000012E-02  -4.69686654E-08
   3.10000014E-02  -8.07182730E-08
   3.20000015E-02  -1.37619722E-07
   3.29999998E-02  -2.32865318E-07
   3.40000018E-02  -3.91131323E-07
   3.50000001E-02  -6.52169206E-07
   3.60000022E-02  -1.07949074E-06
   3.70000005E-02  -1.77374193E-06
   3.80000025E-02  -2.89311424E-06
   3.90000008E-02  -4.68421786E-06
   4.00000028E-02  -7.52835194E-06
   4.10000011E-02  -1.20102159E-05
   4.20000032E-02  -1.90189876E-05
   4.30000015E-02  -2.98956165E-05
   4.40000035E-02  -4.66454076E-05
   4.50000018E-02  -7.22418627E-05
   4.60000038E-02  -1.11056674E-04
   4.70000021E-02  -1.69462219E-04
   4.80000004E-02  -2.56667059E-04
   4.90000024E-02  -3.85862513E-04
   5.00000007E-02  -5.75779588E-04
   5.10000028E-02  -8.52779835E-04
   5.20000011E-02  -1.25363190E-03
   5.30000031E-02  -1.82915619E-03
   5.40000014E-02  -2.64895195E-03
   5.50000034E-02  -3.80745274E-03
   5.60000017E-02  -5.43158222E-03
   5.70000038E-02  -7.69030396E-03
   5.80000021E-02  -1.08063594E-02
   5.90000041E-02  -1.50704822E-02
   6.00000024E-02  -2.08583251E-02
   6.10000044E-02  -2.86502559E-02
   6.20000027E-02  -3.90540585E-02
   6.30000010E-02  -5.28303757E-02
   6.40000030E-02  -7.09205046E-02
   6.50000051E-02  -9.44758207E-02
   6.59999996E-02 -0.124887750    
   6.70000017E-02 -0.163816735    
   6.80000037E-02 -0.213218153    
   6.90000057E-02 -0.275362641    
   7.00000003E-02 -0.352847904    
   7.10000023E-02 -0.448598206    
   7.20000044E-02 -0.565848112    
   7.30000064E-02 -0.708106220    
   7.40000010E-02 -0.879095197    
   7.50000030E-02  -1.08266461    
   7.60000050E-02  -1.32267392    
   7.70000070E-02  -1.60284412    
   7.80000016E-02  -1.92657840    
   7.90000036E-02  -2.29675412    
   8.00000057E-02  -2.71549058    
   8.10000002E-02  -3.18390131    
   8.20000023E-02  -3.70183849    
   8.30000043E-02  -4.26764536    
   8.40000063E-02  -4.87792969    
   8.50000009E-02  -5.52737713    
   8.60000029E-02  -6.20861959    
   8.70000049E-02  -6.91217995    
   8.80000070E-02  -7.62650585    
   8.90000015E-02  -8.33810711    
   9.00000036E-02  -9.03180313    
   9.10000056E-02  -9.69108486    
   9.20000076E-02  -10.2985868    
   9.30000022E-02  -10.8366537    
   9.40000042E-02  -11.2879868    
   9.50000063E-02  -11.6363420    
   9.60000008E-02  -11.8672466    
   9.70000029E-02  -11.9686937    
   9.80000049E-02  -11.9317799    
   9.90000069E-02  -11.7512426    
  0.100000001      -11.4258642    
  0.101000004      -10.9587088    
  0.102000006      -10.3571720    
  0.103000008      -9.63283348    
  0.104000002      -8.80110931    
  0.105000004      -7.88072109    
  0.106000006      -6.89300251    
  0.107000008      -5.86108208    
  0.108000003      -4.80898333    
  0.109000005      -3.76069260    
  0.110000007      -2.73924494    
  0.111000009      -1.76587570    
  0.112000003     -0.859283149    
  0.113000005      -3.50379720E-02
  0.114000008      0.694835067    
  0.115000002       1.32208872    
  0.116000004       1.84229612    
  0.117000006       2.25471520    
  0.118000008       2.56199408    
  0.119000003       2.76974583    
  0.120000005       2.88602757    
  0.121000007       2.92075992    
  0.122000009       2.88512397    
  0.123000003       2.79097009    
  0.124000005       2.65026927    
  0.125000000       2.47463083    
  0.126000002       2.27490306    
  0.127000004       2.06086707    
  0.128000006       1.84102702    
  0.129000008       1.62249207    
  0.130000010       1.41094232    
  0.131000012       1.21066666    
  0.131999999       1.02465737    
  0.133000001      0.854746819    
  0.134000003      0.701771438    
  0.135000005      0.565749228    
  0.136000007      0.446059138    
  0.137000009      0.341613114    
  0.138000011      0.251013547    
  0.139000013      0.172691107    
  0.140000001      0.105020382    
  0.141000003       4.64122891E-02
  0.142000005      -4.61597694E-03
  0.143000007      -4.93915044E-02
  0.144000009      -8.90540481E-02
  0.145000011     -0.124539293    
  0.146000013     -0.156574160    
  0.147000000     -0.185681745    
  0.148000002     -0.212194011    
  0.149000004     -0.236270815    
  0.150000006     -0.257923961    
  0.151000008     -0.277045280    
  0.152000010     -0.293437064    
  0.153000012     -0.306843907    
  0.154000014     -0.316984206    
  0.155000001     -0.323580235    
  0.156000003     -0.326385826    
  0.157000005     -0.325210989    
  0.158000007     -0.319942921    
  0.159000009     -0.310562998    
  0.160000011     -0.297159195    
  0.161000013     -0.279933274    
  0.162000000     -0.259201914    
  0.163000003     -0.235391513    
  0.164000005     -0.209026352    
  0.165000007     -0.180710852    
  0.166000009     -0.151106864    
  0.167000011     -0.120907530    
  0.168000013      -9.08094049E-02
  0.169000015      -6.14844710E-02
  0.170000002      -3.35535184E-02
  0.171000004      -7.56206922E-03
  0.172000006       1.60400663E-02
  0.173000008       3.69146764E-02
  0.174000010       5.48456274E-02
  0.175000012       6.97432160E-02
  0.176000014       8.16421360E-02
  0.177000001       9.06932354E-02
  0.178000003       9.71498787E-02
  0.179000005      0.101350240    
  0.180000007      0.103697196    
  0.181000009      0.104637377    
  0.182000011      0.104640692    
  0.183000013      0.104181141    
  0.184000015      0.103719279    
  0.185000002      0.103686512    
  0.186000004      0.104471296    
  0.187000006      0.106407501    
  0.188000008      0.109765276    
  0.189000010      0.114744805    
  0.190000013      0.121473089    
  0.191000015      0.130003497    
  0.192000002      0.140317529    
  0.193000004      0.152327776    
  0.194000006      0.165881336    
  0.195000008      0.180763200    
  0.196000010      0.196699500    
  0.197000012      0.213361233    
  0.198000014      0.230369180    
  0.199000016      0.247300714    
  0.200000003      0.263699025    
  0.201000005      0.279084653    
  0.202000007      0.292968959    
  0.203000009      0.304868877    
  0.204000011      0.314322412    
  0.205000013      0.320904404    
  0.206000015      0.324242473    
  0.207000017      0.324032903    
  0.208000004      0.320055872    
  0.209000006      0.312189400    
  0.210000008      0.300420880    
  0.211000010      0.284854710    
  0.212000012      0.265714943    
  0.213000014      0.243342265    
  0.214000016      0.218185067    
  0.215000004      0.190785229    
  0.216000006      0.161759377    
  0.217000008      0.131776795    
  0.218000010      0.101534858    
  0.219000012       7.17330500E-02
  0.220000014       4.30463254E-02
  0.221000016       1.60989705E-02
  0.222000018      -8.55977926E-03
  0.223000005      -3.04767732E-02
  0.224000007      -4.93110381E-02
  0.225000009      -6.48435950E-02
  0.226000011      -7.69803375E-02
  0.227000013      -8.57480168E-02
  0.228000015      -9.12841782E-02
  0.229000017      -9.38224494E-02
  0.230000004      -9.36746076E-02
  0.231000006      -9.12108421E-02
  0.232000008      -8.68391991E-02
  0.233000010      -8.09849799E-02
  0.234000012      -7.40707517E-02
  0.235000014      -6.64977580E-02
  0.236000016      -5.86297438E-02
  0.237000018      -5.07802814E-02
  0.238000005      -4.32046130E-02
  0.239000008      -3.60964946E-02
  0.240000010      -2.95898374E-02
  0.241000012      -2.37642378E-02
  0.242000014      -1.86529644E-02
  0.243000016      -1.42518738E-02
  0.244000018      -1.05279991E-02
  0.245000005      -7.42713735E-03
  0.246000007      -4.88035474E-03
  0.247000009      -2.80973758E-03
  0.248000011      -1.13378151E-03
  0.249000013       2.27412063E-04
  0.250000000       1.34734658E-03
  0.251000017       2.28926865E-03
  0.252000004       3.10381199E-03
  0.253000021       3.82849085E-03
  0.254000008       4.48893290E-03
  0.255000025       5.10128308E-03
  0.256000012       5.67493262E-03
  0.256999999       6.21475559E-03
  0.258000016       6.72236551E-03
  0.259000003       7.19638262E-03
  0.260000020       7.63213634E-03
  0.261000007       8.02143849E-03
  0.262000024       8.35296605E-03
  0.263000011       8.61346908E-03
  0.263999999       8.78960546E-03
  0.265000015       8.86988826E-03
  0.266000003       8.84617586E-03
  0.267000020       8.71430617E-03
  0.268000007       8.47383402E-03
  0.269000024       8.12718179E-03
  0.270000011       7.67872017E-03
  0.271000028       7.13425176E-03
  0.272000015       6.50113728E-03
  0.273000002       5.78895723E-03
  0.274000019       5.01032267E-03
  0.275000006       4.18134639E-03
  0.276000023       3.32139735E-03
  0.277000010       2.45204126E-03
  0.278000027       1.59538304E-03
  0.279000014       7.72249536E-04
  0.280000001       6.88847649E-07
  0.281000018      -7.04886217E-04
  0.282000005      -1.33384834E-03
  0.283000022      -1.87899917E-03
  0.284000009      -2.33605527E-03
  0.285000026      -2.70331348E-03
  0.286000013      -2.98166834E-03
  0.287000000      -3.17493035E-03
  0.288000017      -3.29020340E-03
  0.289000005      -3.33801494E-03
  0.290000021      -3.33196600E-03
  0.291000009      -3.28785297E-03
  0.292000026      -3.22242011E-03
  0.293000013      -3.15203820E-03
  0.294000000      -3.09162471E-03
  0.295000017      -3.05401115E-03
  0.296000004      -3.04977898E-03
  0.297000021      -3.08741536E-03
  0.298000008      -3.17354198E-03
  0.299000025      -3.31299473E-03
  0.300000012      -3.50865303E-03
  0.301000029      -3.76107614E-03
  0.302000016      -4.06813016E-03
  0.303000003      -4.42482298E-03
  0.304000020      -4.82349005E-03
  0.305000007      -5.25433477E-03
  0.306000024      -5.70617663E-03
  0.307000011      -6.16718363E-03
  0.308000028      -6.62537431E-03
  0.309000015      -7.06879655E-03
  0.310000002      -7.48544000E-03
  0.311000019      -7.86307082E-03
  0.312000006      -8.18921160E-03
  0.313000023      -8.45143571E-03
  0.314000010      -8.63798428E-03
  0.315000027      -8.73857457E-03
  0.316000015      -8.74515809E-03
  0.317000002      -8.65239650E-03
  0.318000019      -8.45774170E-03
  0.319000006      -8.16116948E-03
  0.320000023      -7.76476134E-03
  0.321000010      -7.27239670E-03
  0.322000027      -6.68974919E-03
  0.323000014      -6.02462888E-03
  0.324000001      -5.28752618E-03
  0.325000018      -4.49207937E-03
  0.326000005      -3.65517382E-03
  0.327000022      -2.79647834E-03
  0.328000009      -1.93742057E-03
  0.329000026      -1.09978952E-03
  0.330000013      -3.04276036E-04
  0.331000030       4.30742366E-04
  0.332000017       1.08997826E-03
  0.333000004       1.66155049E-03
  0.334000021       2.13697087E-03
  0.335000008       2.51100538E-03
  0.336000025       2.78158020E-03
  0.337000012       2.94979871E-03
  0.338000029       3.02000763E-03
  0.339000016       2.99977115E-03
  0.340000004       2.89961416E-03
  0.341000021       2.73246970E-03
  0.342000008       2.51287501E-03
  0.343000025       2.25604512E-03
  0.344000012       1.97697640E-03
  0.345000029       1.68969051E-03
  0.346000016       1.40665239E-03
  0.347000003       1.13833032E-03
  0.348000020       8.92851967E-04
  0.349000007       6.75747637E-04
  0.350000024       4.89835744E-04
  0.351000011       3.35344172E-04
  0.352000028       2.10337763E-04
  0.353000015       1.11432062E-04
  0.354000002       3.46536253E-05
  0.355000019      -2.37812346E-05
  0.356000006      -6.69976871E-05
  0.357000023      -9.74268260E-05
  0.358000010      -1.17150659E-04
  0.359000027      -1.28404907E-04
  0.360000014      -1.33936162E-04
  0.361000031      -1.36967996E-04
  0.362000018      -1.40713135E-04
  0.363000005      -1.47600134E-04
  0.364000022      -1.58555209E-04
  0.365000010      -1.72704255E-04
  0.366000026      -1.87710699E-04
  0.367000014      -2.00696217E-04
  0.368000031      -2.09419130E-04
  0.369000018      -2.13235922E-04
  0.370000005      -2.13423555E-04
  0.371000022      -2.12685190E-04
  0.372000009      -2.14002546E-04
  0.373000026      -2.19288238E-04
  0.374000013      -2.28400313E-04
  0.375000030      -2.38952925E-04
  0.376000017      -2.47038843E-04
  0.377000004      -2.48602650E-04
  0.378000021      -2.40928770E-04
  0.379000008      -2.23654366E-04
  0.380000025      -1.98907641E-04
  0.381000012      -1.70526866E-04
  0.382000029      -1.42683668E-04
  0.383000016      -1.18461401E-04
  0.384000003      -9.89351392E-05
  0.385000020      -8.30738427E-05
  0.386000007      -6.84413753E-05
  0.387000024      -5.23602503E-05
  0.388000011      -3.30479452E-05
  0.389000028      -1.02927688E-05
  0.390000015       1.45366257E-05
  0.391000032       3.90674213E-05
  0.392000020       6.07841357E-05
  0.393000007       7.78074173E-05
  0.394000024       8.93162724E-05
  0.395000011       9.55332580E-05
  0.396000028       9.73854694E-05
  0.397000015       9.60609686E-05
  0.398000032       9.26664652E-05
  0.399000019       8.80857478E-05
  0.400000006       8.30091303E-05
  0.401000023       7.80230621E-05
  0.402000010       7.36518632E-05
  0.403000027       7.03149635E-05
  0.404000014       6.82494865E-05
  0.405000031       6.74907205E-05
  0.406000018       6.79725417E-05
  0.407000005       6.97233991E-05
  0.408000022       7.30454631E-05
  0.409000009       7.85350639E-05
  0.410000026       8.68623247E-05
  0.411000013       9.83560385E-05
  0.412000030       1.12571928E-04
  0.413000017       1.28081520E-04
  0.414000034       1.42658260E-04
  0.415000021       1.53868707E-04
  0.416000009       1.59872114E-04
  0.417000026       1.60090989E-04
  0.418000013       1.55418777E-04
  0.419000030       1.47795392E-04
  0.420000017       1.39245807E-04
  0.421000034       1.30725995E-04
  0.422000021       1.21235258E-04
  0.423000008       1.07569787E-04
  0.424000025       8.48335549E-05
  0.425000012       4.74959015E-05
  0.426000029      -9.46453656E-06
  0.427000016      -8.98467697E-05
  0.428000033      -1.96293142E-04
  0.429000020      -3.31155199E-04
  0.430000007      -4.97840520E-04
  0.431000024      -7.02118035E-04
  0.432000011      -9.52889444E-04
  0.433000028      -1.26218318E-03
  0.434000015      -1.64448773E-03
  0.435000032      -2.11584708E-03
  0.436000019      -2.69325450E-03
  0.437000006      -3.39474552E-03
  0.438000023      -4.24027210E-03
  0.439000010      -5.25307423E-03
  0.440000027      -6.46102475E-03
  0.441000015      -7.89741892E-03
  0.442000031      -9.60090477E-03
  0.443000019      -1.16146123E-02
  0.444000036      -1.39848674E-02
  0.445000023      -1.67600121E-02
  0.446000010      -1.99897457E-02
  0.447000027      -2.37250961E-02
  0.448000014      -2.80187484E-02
  0.449000031      -3.29251960E-02
  0.450000018      -3.85001488E-02
  0.451000035      -4.47988659E-02
  0.452000022      -5.18733971E-02
  0.453000009      -5.97691275E-02
  0.454000026      -6.85211867E-02
  0.455000013      -7.81512186E-02
  0.456000030      -8.86647776E-02
  0.457000017     -0.100049272    
  0.458000034     -0.112272054    
  0.459000021     -0.125278279    
  0.460000008     -0.138988361    
  0.461000025     -0.153295025    
  0.462000012     -0.168060526    
  0.463000029     -0.183114678    
  0.464000016     -0.198254257    
  0.465000033     -0.213244215    
  0.466000021     -0.227820694    
  0.467000008     -0.241695568    
  0.468000025     -0.254562140    
  0.469000012     -0.266101599    
  0.470000029     -0.275990218    
  0.471000016     -0.283907413    
  0.472000033     -0.289544880    
  0.473000020     -0.292616934    
  0.474000037     -0.292871922    
  0.475000024     -0.290104300    
  0.476000011     -0.284166187    
  0.477000028     -0.274977833    
  0.478000015     -0.262535751    
  0.479000032     -0.246918336    
  0.480000019     -0.228288442    
  0.481000036     -0.206893176    
  0.482000023     -0.183060914    
  0.483000010     -0.157195687    
  0.484000027     -0.129768580    
  0.485000014     -0.101306029    
  0.486000031      -7.23746940E-02
  0.487000018      -4.35630269E-02
  0.488000035      -1.54600870E-02
  0.489000022       1.13672875E-02
  0.490000010       3.63974981E-02
  0.491000026       5.91757819E-02
  0.492000014       7.93329626E-02
  0.493000031       9.66003239E-02
  0.494000018      0.110819943    
  0.495000035      0.121950336    
  0.496000022      0.130067363    
  0.497000009      0.135360479    
  0.498000026      0.138124302    
  0.499000013      0.138745606    
  0.500000000      0.137685850    
  0.501000047      0.135459751    
  0.502000034      0.132610917    
  0.503000021      0.129685864    
  0.504000008      0.127208069    
  0.504999995      0.125653818    
  0.506000042      0.125431165    
  0.507000029      0.126863167    
  0.508000016      0.130175918    
  0.509000003      0.135491446    
  0.510000050      0.142825484    
  0.511000037      0.152089730    
  0.512000024      0.163098335    
  0.513000011      0.175578237    
  0.513999999      0.189182729    
  0.515000045      0.203507587    
  0.516000032      0.218108669    
  0.517000020      0.232519880    
  0.518000007      0.246270403    
  0.519000053      0.258900195    
  0.520000041      0.269973308    
  0.521000028      0.279088795    
  0.522000015      0.285889417    
  0.523000002      0.290068746    
  0.524000049      0.291376978    
  0.525000036      0.289625913    
  0.526000023      0.284692973    
  0.527000010      0.276524603    
  0.527999997      0.265138596    
  0.529000044      0.250625432    
  0.530000031      0.233148649    
  0.531000018      0.212944046    
  0.532000005      0.190317795    
  0.533000052      0.165643111    
  0.534000039      0.139355183    
  0.535000026      0.111943774    
  0.536000013       8.39431509E-02
  0.537000000       5.59189543E-02
  0.538000047       2.84522045E-02
  0.539000034       2.12089205E-03
  0.540000021      -2.25199368E-02
  0.541000009      -4.49584834E-02
  0.542000055      -6.47456720E-02
  0.543000042      -8.15126672E-02
  0.544000030      -9.49854851E-02
  0.545000017     -0.104995884    
  0.546000004     -0.111488000    
  0.547000051     -0.114520244    
  0.548000038     -0.114262208    
  0.549000025     -0.110986441    
  0.550000012     -0.105055347    
  0.550999999      -9.69036669E-02
  0.552000046      -8.70174989E-02
  0.553000033      -7.59110451E-02
  0.554000020      -6.41025379E-02
  0.555000007      -5.20908348E-02
  0.556000054      -4.03340757E-02
  0.557000041      -2.92315651E-02
  0.558000028      -1.91097260E-02
  0.559000015      -1.02126915E-02
  0.560000002      -2.69779190E-03
  0.561000049       3.36407591E-03
  0.562000036       7.98343867E-03
  0.563000023       1.12430844E-02
  0.564000010       1.32854730E-02
  0.564999998       1.42976502E-02
  0.566000044       1.44947721E-02
  0.567000031       1.41034089E-02
  0.568000019       1.33457808E-02
  0.569000006       1.24259368E-02
  0.570000052       1.15186693E-02
  0.571000040       1.07616819E-02
  0.572000027       1.02512361E-02
  0.573000014       1.00412359E-02
  0.574000001       1.01454603E-02
  0.575000048       1.05424533E-02
  0.576000035       1.11824330E-02
  0.577000022       1.19954664E-02
  0.578000009       1.29001364E-02
  0.579000056       1.38119310E-02
  0.580000043       1.46506988E-02
  0.581000030       1.53466556E-02
  0.582000017       1.58446096E-02
  0.583000004       1.61062777E-02
  0.584000051       1.61107555E-02
  0.585000038       1.58533528E-02
  0.586000025       1.53431678E-02
  0.587000012       1.45998402E-02
  0.588000000       1.36499880E-02
  0.589000046       1.25238271E-02
  0.590000033       1.12523902E-02
  0.591000021       9.86563321E-03
  0.592000008       8.39152373E-03
  0.593000054       6.85599633E-03
  0.594000041       5.28349867E-03
  0.595000029       3.69776017E-03
  0.596000016       2.12243432E-03
  0.597000003       5.81372704E-04
  0.598000050      -9.01543011E-04
  0.599000037      -2.30293232E-03
  0.600000024      -3.60064115E-03
  0.601000011      -4.77449084E-03
  0.602000058      -5.80696156E-03
  0.603000045      -6.68382598E-03
  0.604000032      -7.39482604E-03
  0.605000019      -7.93447997E-03
  0.606000006      -8.30302108E-03
  0.607000053      -8.50732811E-03
  0.608000040      -8.56158696E-03
  0.609000027      -8.48738477E-03
  0.610000014      -8.31301138E-03
  0.611000001      -8.07190966E-03
  0.612000048      -7.80044589E-03
  0.613000035      -7.53533654E-03
  0.614000022      -7.31114857E-03
  0.615000010      -7.15823192E-03
  0.616000056      -7.10128527E-03
  0.617000043      -7.15856720E-03
  0.618000031      -7.34161818E-03
  0.619000018      -7.65530625E-03
  0.620000005      -8.09805188E-03
  0.621000051      -8.66219308E-03
  0.622000039      -9.33453068E-03
  0.623000026      -1.00971479E-02
  0.624000013      -1.09285293E-02
  0.625000000      -1.18049048E-02
  0.626000047      -1.27016455E-02
  0.627000034      -1.35945091E-02
  0.628000021      -1.44605804E-02
  0.629000008      -1.52788954E-02
  0.630000055      -1.60308871E-02
  0.631000042      -1.67008769E-02
  0.632000029      -1.72768235E-02
  0.633000016      -1.77514087E-02
  0.634000003      -1.81233454E-02
  0.635000050      -1.83986351E-02
  0.636000037      -1.85914189E-02
  0.637000024      -1.87241677E-02
  0.638000011      -1.88271571E-02
  0.639000058      -1.89373977E-02
  0.640000045      -1.90973934E-02
  0.641000032      -1.93541292E-02
  0.642000020      -1.97585821E-02
  0.643000007      -2.03657951E-02
  0.644000053      -2.12352909E-02
  0.645000041      -2.24314388E-02
  0.646000028      -2.40233205E-02
  0.647000015      -2.60838214E-02
  0.648000002      -2.86878776E-02
  0.649000049      -3.19100618E-02
  0.650000036      -3.58218662E-02
  0.651000023      -4.04890329E-02
  0.652000010      -4.59691435E-02
  0.653000057      -5.23095503E-02
  0.654000044      -5.95454723E-02
  0.655000031      -6.76980615E-02
  0.656000018      -7.67722502E-02
  0.657000005      -8.67543593E-02
  0.658000052      -9.76095870E-02
  0.659000039     -0.109279729    
  0.660000026     -0.121681400    
  0.661000013     -0.134705037    
  0.662000060     -0.148214832    
  0.663000047     -0.162049457    
  0.664000034     -0.176023558    
  0.665000021     -0.189929739    
  0.666000009     -0.203540966    
  0.667000055     -0.216613427    
  0.668000042     -0.228889942    
  0.669000030     -0.240104124    
  0.670000017     -0.249985352    
  0.671000004     -0.258264691    
  0.672000051     -0.264681518    
  0.673000038     -0.268990785    
  0.674000025     -0.270970613    
  0.675000012     -0.270429879    
  0.676000059     -0.267215461    
  0.677000046     -0.261219144    
  0.678000033     -0.252383828    
  0.679000020     -0.240708962    
  0.680000007     -0.226254940    
  0.681000054     -0.209146157    
  0.682000041     -0.189572498    
  0.683000028     -0.167788863    
  0.684000015     -0.144112438    
  0.685000062     -0.118917465    
  0.686000049      -9.26274583E-02
  0.687000036      -6.57049045E-02
  0.688000023      -3.86386625E-02
  0.689000010      -1.19294263E-02
  0.690000057       1.39262378E-02
  0.691000044       3.84526849E-02
  0.692000031       6.12121634E-02
  0.693000019       8.18212256E-02
  0.694000006       9.99658778E-02
  0.695000052      0.115414724    
  0.696000040      0.128029376    
  0.697000027      0.137771428    
  0.698000014      0.144705549    
e