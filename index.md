---
title       : Patient Show up prediction  
subtitle    : 
author      : demo 
job         : Data Mining
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

 


* / >1.Predictions using ML algorithms
* / >2.Data Visulaisation

---

```r
medical$No.show<-as.factor(medical$No.show)
medical$Gender<-as.factor(medical$Gender)
medical$Scholarship<-as.factor(medical$Scholarship)
na.omit(medical)
```

```
##         PatientId AppointmentID Gender Age       Neighbourhood Scholarship
## 1    2.990000e+13       5642903      F  62     JARDIM DA PENHA           0
## 2    5.590000e+14       5642503      M  56     JARDIM DA PENHA           0
## 3    4.260000e+12       5642549      F  62       MATA DA PRAIA           0
## 4    8.680000e+11       5642828      F   8   PONTAL DE CAMBURI           0
## 5    8.840000e+12       5642494      F  56     JARDIM DA PENHA           0
## 6    9.600000e+13       5626772      F  76          REP�<U+009A>BLICA           0
## 7    7.340000e+14       5630279      F  23          GOIABEIRAS           0
## 8    3.450000e+12       5630575      F  39          GOIABEIRAS           0
## 9    5.640000e+13       5638447      F  21          ANDORINHAS           0
## 10   7.810000e+13       5629123      F  19           CONQUISTA           0
## 11   7.350000e+14       5630213      F  30       0VA PALESTINA           0
## 12   7.540000e+12       5620163      M  29       0VA PALESTINA           0
## 13   5.670000e+14       5634718      F  22       0VA PALESTINA           1
## 14   9.110000e+14       5636249      M  28       0VA PALESTINA           0
## 15   9.990000e+13       5633951      F  54       0VA PALESTINA           0
## 16   9.994839e+10       5620206      F  15       0VA PALESTINA           0
## 17   8.460000e+13       5633121      M  50       0VA PALESTINA           0
## 18   1.480000e+13       5633460      F  40           CONQUISTA           1
## 19   1.710000e+13       5621836      F  30       0VA PALESTINA           1
## 20   7.220000e+12       5640433      F  46            DA PENHA           0
## 21   6.220000e+14       5626083      F  30       0VA PALESTINA           0
## 22   1.220000e+13       5628338      F   4           CONQUISTA           0
## 23   8.630000e+14       5616091      M  13           CONQUISTA           0
## 24   2.140000e+14       5634142      F  46           CONQUISTA           0
## 25   8.730000e+12       5641780      F  65          TABUAZEIRO           0
## 26   5.820000e+12       5624020      M  46           CONQUISTA           0
## 27   2.578785e+10       5641781      F  45      BENTO FERREIRA           0
## 28   1.220000e+13       5628345      F   4           CONQUISTA           0
## 29   5.930000e+12       5642400      M  51          S�<U+0083>O PEDRO           0
## 30   1.230000e+12       5642186      F  32        SANTA MARTHA           0
## 31   3.430000e+11       5628068      F  46       0VA PALESTINA           0
## 32   3.110000e+11       5628907      M  12       0VA PALESTINA           1
## 33   5.290000e+13       5637908      M  61    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 34   7.650000e+12       5616921      F  38    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           1
## 35   2.000000e+13       5637963      F  79    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 36   7.820000e+13       5637968      M  18    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 37   7.300000e+13       5637975      F  63    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 38   1.580000e+12       5637986      F  64          TABUAZEIRO           1
## 39   5.870000e+12       5609446      M  85    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 40   1.460000e+13       5639644      F  59    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 41   9.970000e+14       5635881      F  55          TABUAZEIRO           0
## 42   8.220000e+14       5633339      F  71            MARUÍPE           0
## 43   2.600000e+13       5632906      F  50    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 44   7.160000e+13       5641620      F  49    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 45   2.740000e+11       5635414      F  78    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 46   4.980000e+12       5635842      F  31    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 47   1.380000e+11       5615608      M  58    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 48   5.890000e+11       5633116      F  39            MARUÍPE           0
## 49   8.550000e+12       5618643      F  58    S�<U+0083>O CRIST�<U+0093>V�<U+0083>O           0
## 50   9.220000e+13       5534656      F  27     GRANDE VIT�<U+0093>RIA           0
## 51   1.830000e+14       5534661      F  19     GRANDE VIT�<U+0093>RIA           0
## 52   4.690000e+13       5534635      F  23     GRANDE VIT�<U+0093>RIA           1
## 53   7.990000e+14       5534639      F  23     GRANDE VIT�<U+0093>RIA           1
## 54   4.440000e+14       5637909      M   6           CONQUISTA           0
## 55   3.370000e+12       5638164      F   6       0VA PALESTINA           0
## 56   4.180000e+13       5638018      M   2       0VA PALESTINA           0
## 57   4.750000e+14       5600005      M  12       0VA PALESTINA           0
## 58   4.000000e+12       5638466      M  11           CONQUISTA           0
## 59   4.487944e+10       5638235      M   7          S�<U+0083>O PEDRO           0
## 60   7.180000e+13       5638545      F   0           CONQUISTA           0
## 61   9.290000e+12       5628739      M   8       0VA PALESTINA           1
## 62   5.590000e+11       5626971      F   2       0VA PALESTINA           0
## 63   3.650000e+13       5614045      F   3           CONQUISTA           1
## 64   2.370000e+14       5628286      M   0       S�<U+0083>O BENEDITO           0
## 65   1.890000e+14       5616082      M   0   ILHA DAS CAIEIRAS           0
## 66   2.720000e+14       5628321      M   0           CONQUISTA           0
## 67   9.780000e+14       5638604      M   1           CONQUISTA           0
## 68   8.650000e+13       5639264      F   0       0VA PALESTINA           0
## 69   5.430000e+12       5552915      F  69     JARDIM DA PENHA           0
## 70   7.940000e+11       5552917      F  58        SANTO ANDR�<U+0089>           0
## 71   6.710000e+13       5552914      M  62        SOLON BORGES           0
## 72   1.850000e+12       5552936      F  30              BONFIM           1
## 73   7.750000e+12       5638014      F  61      JARDIM CAMBURI           0
## 74   4.540000e+13       5552934      F  68          REP�<U+009A>BLICA           0
## 75   9.670000e+12       5597628      F  64         MARIA ORTIZ           0
## 76   1.490000e+11       5597632      F  60              JABOUR           0
## 77   6.550000e+12       5597643      M  28   ANT�<U+0094>NIO HON�<U+0093>RIO           0
## 78   5.750000e+12       5642767      F  27              JABOUR           0
## 79   6.260000e+11       5597672      M  21         MARIA ORTIZ           0
## 80   9.910000e+13       5597673      M  67         MARIA ORTIZ           0
## 81   1.490000e+12       5597685      M  68              JABOUR           0
## 82   1.980000e+13       5597689      F  49              JABOUR           0
## 83   1.830000e+11       5638939      M  71              JABOUR           0
##      Hipertension Diabetes Alcoholism Handcap SMS_received No.show
## 1               1        0          0       0            0       0
## 2               0        0          0       0            0       0
## 3               0        0          0       0            0       0
## 4               0        0          0       0            0       0
## 5               1        1          0       0            0       0
## 6               1        0          0       0            0       0
## 7               0        0          0       0            0       1
## 8               0        0          0       0            0       1
## 9               0        0          0       0            0       0
## 10              0        0          0       0            0       0
## 11              0        0          0       0            0       0
## 12              0        0          0       0            1       1
## 13              0        0          0       0            0       0
## 14              0        0          0       0            0       0
## 15              0        0          0       0            0       0
## 16              0        0          0       0            1       0
## 17              0        0          0       0            0       0
## 18              0        0          0       0            0       1
## 19              0        0          0       0            1       0
## 20              0        0          0       0            0       0
## 21              0        0          0       0            0       1
## 22              0        0          0       0            0       1
## 23              0        0          0       0            1       1
## 24              0        0          0       0            0       0
## 25              0        0          0       0            0       0
## 26              1        0          0       0            1       0
## 27              1        0          0       0            0       0
## 28              0        0          0       0            0       0
## 29              0        0          0       0            0       0
## 30              0        0          0       0            0       0
## 31              0        0          0       0            0       0
## 32              0        0          0       0            0       1
## 33              1        0          0       0            0       0
## 34              0        0          0       0            1       0
## 35              1        0          0       0            0       0
## 36              0        0          0       0            0       0
## 37              1        1          0       0            0       0
## 38              1        1          0       0            0       0
## 39              1        0          0       0            1       0
## 40              0        0          0       0            0       0
## 41              0        0          0       0            0       0
## 42              0        1          0       0            0       0
## 43              0        0          0       0            0       0
## 44              1        0          0       0            0       0
## 45              1        1          0       0            0       1
## 46              0        0          0       0            0       0
## 47              1        0          1       0            1       0
## 48              1        1          0       0            0       0
## 49              0        0          0       0            1       1
## 50              0        0          0       0            1       1
## 51              0        0          0       0            1       1
## 52              0        0          0       0            1       1
## 53              0        0          0       0            1       1
## 54              0        0          0       0            0       0
## 55              0        0          0       0            0       0
## 56              0        0          0       0            0       0
## 57              0        0          0       0            1       1
## 58              0        0          0       0            0       0
## 59              0        0          0       0            0       0
## 60              0        0          0       0            0       0
## 61              0        0          0       0            0       0
## 62              0        0          0       0            0       1
## 63              0        0          0       0            1       0
## 64              0        0          0       0            0       0
## 65              0        0          0       0            1       0
## 66              0        0          0       0            0       0
## 67              0        0          0       0            0       0
## 68              0        0          0       0            0       0
## 69              1        0          0       0            1       0
## 70              0        0          0       0            1       0
## 71              0        0          0       0            0       0
## 72              0        0          0       0            1       0
## 73              0        0          0       0            0       0
## 74              1        1          0       0            1       0
## 75              0        0          0       0            1       0
## 76              0        0          0       0            0       0
## 77              0        0          0       0            0       1
## 78              0        0          0       0            0       0
## 79              0        0          0       0            1       0
## 80              0        0          0       0            1       1
## 81              0        0          0       0            1       0
## 82              0        0          0       0            1       0
## 83              0        0          0       0            0       0
##  [ reached getOption("max.print") -- omitted 9916 rows ]
```
---

```r
library(caret)
Train <- createDataPartition(medical$No.show, p=0.7, list=FALSE)
na.omit(Train)
```

```
##          Resample1
##     [1,]         1
##     [2,]         2
##     [3,]         3
##     [4,]         4
##     [5,]         5
##     [6,]         7
##     [7,]         8
##     [8,]         9
##     [9,]        11
##    [10,]        13
##    [11,]        15
##    [12,]        16
##    [13,]        17
##    [14,]        18
##    [15,]        19
##    [16,]        21
##    [17,]        23
##    [18,]        24
##    [19,]        25
##    [20,]        26
##    [21,]        28
##    [22,]        30
##    [23,]        31
##    [24,]        32
##    [25,]        33
##    [26,]        35
##    [27,]        36
##    [28,]        37
##    [29,]        38
##    [30,]        40
##    [31,]        41
##    [32,]        43
##    [33,]        46
##    [34,]        47
##    [35,]        48
##    [36,]        49
##    [37,]        50
##    [38,]        51
##    [39,]        52
##    [40,]        53
##    [41,]        55
##    [42,]        56
##    [43,]        57
##    [44,]        58
##    [45,]        59
##    [46,]        60
##    [47,]        62
##    [48,]        63
##    [49,]        65
##    [50,]        66
##    [51,]        67
##    [52,]        68
##    [53,]        69
##    [54,]        70
##    [55,]        72
##    [56,]        73
##    [57,]        74
##    [58,]        76
##    [59,]        78
##    [60,]        79
##    [61,]        80
##    [62,]        81
##    [63,]        85
##    [64,]        86
##    [65,]        87
##    [66,]        88
##    [67,]        89
##    [68,]        90
##    [69,]        94
##    [70,]        95
##    [71,]        99
##    [72,]       100
##    [73,]       101
##    [74,]       103
##    [75,]       104
##    [76,]       105
##    [77,]       106
##    [78,]       107
##    [79,]       110
##    [80,]       111
##    [81,]       112
##    [82,]       114
##    [83,]       115
##    [84,]       116
##    [85,]       120
##    [86,]       121
##    [87,]       122
##    [88,]       123
##    [89,]       124
##    [90,]       125
##    [91,]       127
##    [92,]       128
##    [93,]       129
##    [94,]       130
##    [95,]       131
##    [96,]       132
##    [97,]       133
##    [98,]       134
##    [99,]       135
##   [100,]       136
##   [101,]       137
##   [102,]       138
##   [103,]       140
##   [104,]       143
##   [105,]       144
##   [106,]       145
##   [107,]       146
##   [108,]       147
##   [109,]       148
##   [110,]       149
##   [111,]       153
##   [112,]       155
##   [113,]       156
##   [114,]       157
##   [115,]       158
##   [116,]       159
##   [117,]       160
##   [118,]       161
##   [119,]       162
##   [120,]       163
##   [121,]       164
##   [122,]       165
##   [123,]       166
##   [124,]       167
##   [125,]       168
##   [126,]       169
##   [127,]       170
##   [128,]       171
##   [129,]       172
##   [130,]       173
##   [131,]       174
##   [132,]       175
##   [133,]       176
##   [134,]       177
##   [135,]       178
##   [136,]       180
##   [137,]       181
##   [138,]       182
##   [139,]       183
##   [140,]       185
##   [141,]       187
##   [142,]       188
##   [143,]       190
##   [144,]       191
##   [145,]       192
##   [146,]       193
##   [147,]       194
##   [148,]       196
##   [149,]       197
##   [150,]       199
##   [151,]       200
##   [152,]       201
##   [153,]       202
##   [154,]       203
##   [155,]       204
##   [156,]       205
##   [157,]       206
##   [158,]       207
##   [159,]       208
##   [160,]       209
##   [161,]       211
##   [162,]       212
##   [163,]       213
##   [164,]       214
##   [165,]       215
##   [166,]       216
##   [167,]       217
##   [168,]       219
##   [169,]       221
##   [170,]       223
##   [171,]       225
##   [172,]       226
##   [173,]       227
##   [174,]       229
##   [175,]       230
##   [176,]       234
##   [177,]       235
##   [178,]       236
##   [179,]       237
##   [180,]       238
##   [181,]       240
##   [182,]       242
##   [183,]       243
##   [184,]       244
##   [185,]       246
##   [186,]       248
##   [187,]       251
##   [188,]       252
##   [189,]       253
##   [190,]       254
##   [191,]       256
##   [192,]       257
##   [193,]       259
##   [194,]       262
##   [195,]       263
##   [196,]       264
##   [197,]       267
##   [198,]       269
##   [199,]       274
##   [200,]       275
##   [201,]       276
##   [202,]       277
##   [203,]       279
##   [204,]       280
##   [205,]       281
##   [206,]       283
##   [207,]       284
##   [208,]       285
##   [209,]       287
##   [210,]       288
##   [211,]       289
##   [212,]       290
##   [213,]       292
##   [214,]       293
##   [215,]       294
##   [216,]       299
##   [217,]       300
##   [218,]       301
##   [219,]       302
##   [220,]       303
##   [221,]       304
##   [222,]       305
##   [223,]       307
##   [224,]       309
##   [225,]       310
##   [226,]       311
##   [227,]       314
##   [228,]       315
##   [229,]       317
##   [230,]       318
##   [231,]       320
##   [232,]       322
##   [233,]       323
##   [234,]       325
##   [235,]       326
##   [236,]       327
##   [237,]       328
##   [238,]       331
##   [239,]       332
##   [240,]       333
##   [241,]       334
##   [242,]       335
##   [243,]       337
##   [244,]       338
##   [245,]       340
##   [246,]       342
##   [247,]       343
##   [248,]       345
##   [249,]       348
##   [250,]       349
##   [251,]       350
##   [252,]       351
##   [253,]       352
##   [254,]       353
##   [255,]       354
##   [256,]       356
##   [257,]       357
##   [258,]       360
##   [259,]       361
##   [260,]       362
##   [261,]       363
##   [262,]       364
##   [263,]       365
##   [264,]       366
##   [265,]       367
##   [266,]       368
##   [267,]       369
##   [268,]       370
##   [269,]       371
##   [270,]       372
##   [271,]       373
##   [272,]       375
##   [273,]       376
##   [274,]       377
##   [275,]       379
##   [276,]       380
##   [277,]       381
##   [278,]       383
##   [279,]       384
##   [280,]       385
##   [281,]       386
##   [282,]       387
##   [283,]       389
##   [284,]       390
##   [285,]       391
##   [286,]       392
##   [287,]       393
##   [288,]       394
##   [289,]       396
##   [290,]       397
##   [291,]       398
##   [292,]       399
##   [293,]       401
##   [294,]       402
##   [295,]       403
##   [296,]       404
##   [297,]       406
##   [298,]       407
##   [299,]       408
##   [300,]       409
##   [301,]       410
##   [302,]       411
##   [303,]       412
##   [304,]       414
##   [305,]       416
##   [306,]       419
##   [307,]       420
##   [308,]       421
##   [309,]       422
##   [310,]       423
##   [311,]       426
##   [312,]       428
##   [313,]       429
##   [314,]       430
##   [315,]       431
##   [316,]       433
##   [317,]       437
##   [318,]       439
##   [319,]       440
##   [320,]       443
##   [321,]       444
##   [322,]       446
##   [323,]       449
##   [324,]       451
##   [325,]       454
##   [326,]       455
##   [327,]       456
##   [328,]       457
##   [329,]       458
##   [330,]       459
##   [331,]       461
##   [332,]       462
##   [333,]       464
##   [334,]       465
##   [335,]       466
##   [336,]       468
##   [337,]       469
##   [338,]       471
##   [339,]       472
##   [340,]       476
##   [341,]       477
##   [342,]       478
##   [343,]       479
##   [344,]       481
##   [345,]       482
##   [346,]       483
##   [347,]       485
##   [348,]       486
##   [349,]       489
##   [350,]       490
##   [351,]       491
##   [352,]       494
##   [353,]       495
##   [354,]       496
##   [355,]       499
##   [356,]       500
##   [357,]       501
##   [358,]       504
##   [359,]       506
##   [360,]       507
##   [361,]       509
##   [362,]       510
##   [363,]       511
##   [364,]       515
##   [365,]       516
##   [366,]       518
##   [367,]       519
##   [368,]       521
##   [369,]       522
##   [370,]       523
##   [371,]       525
##   [372,]       526
##   [373,]       529
##   [374,]       531
##   [375,]       533
##   [376,]       534
##   [377,]       535
##   [378,]       536
##   [379,]       537
##   [380,]       538
##   [381,]       539
##   [382,]       540
##   [383,]       541
##   [384,]       542
##   [385,]       543
##   [386,]       544
##   [387,]       545
##   [388,]       546
##   [389,]       547
##   [390,]       549
##   [391,]       550
##   [392,]       552
##   [393,]       553
##   [394,]       554
##   [395,]       555
##   [396,]       556
##   [397,]       559
##   [398,]       561
##   [399,]       562
##   [400,]       563
##   [401,]       564
##   [402,]       565
##   [403,]       566
##   [404,]       568
##   [405,]       569
##   [406,]       570
##   [407,]       571
##   [408,]       572
##   [409,]       574
##   [410,]       575
##   [411,]       576
##   [412,]       577
##   [413,]       579
##   [414,]       582
##   [415,]       585
##   [416,]       586
##   [417,]       587
##   [418,]       588
##   [419,]       589
##   [420,]       591
##   [421,]       592
##   [422,]       593
##   [423,]       594
##   [424,]       595
##   [425,]       597
##   [426,]       598
##   [427,]       599
##   [428,]       600
##   [429,]       602
##   [430,]       603
##   [431,]       604
##   [432,]       605
##   [433,]       606
##   [434,]       607
##   [435,]       609
##   [436,]       611
##   [437,]       613
##   [438,]       614
##   [439,]       615
##   [440,]       616
##   [441,]       617
##   [442,]       619
##   [443,]       620
##   [444,]       621
##   [445,]       622
##   [446,]       624
##   [447,]       625
##   [448,]       626
##   [449,]       628
##   [450,]       629
##   [451,]       631
##   [452,]       632
##   [453,]       633
##   [454,]       634
##   [455,]       636
##   [456,]       637
##   [457,]       638
##   [458,]       639
##   [459,]       641
##   [460,]       643
##   [461,]       644
##   [462,]       645
##   [463,]       646
##   [464,]       648
##   [465,]       650
##   [466,]       651
##   [467,]       653
##   [468,]       655
##   [469,]       656
##   [470,]       658
##   [471,]       659
##   [472,]       660
##   [473,]       662
##   [474,]       663
##   [475,]       664
##   [476,]       665
##   [477,]       668
##   [478,]       672
##   [479,]       673
##   [480,]       674
##   [481,]       675
##   [482,]       676
##   [483,]       677
##   [484,]       678
##   [485,]       679
##   [486,]       680
##   [487,]       682
##   [488,]       683
##   [489,]       684
##   [490,]       686
##   [491,]       689
##   [492,]       690
##   [493,]       694
##   [494,]       696
##   [495,]       697
##   [496,]       698
##   [497,]       699
##   [498,]       700
##   [499,]       701
##   [500,]       702
##   [501,]       703
##   [502,]       704
##   [503,]       706
##   [504,]       707
##   [505,]       708
##   [506,]       710
##   [507,]       711
##   [508,]       712
##   [509,]       714
##   [510,]       719
##   [511,]       724
##   [512,]       726
##   [513,]       727
##   [514,]       728
##   [515,]       730
##   [516,]       731
##   [517,]       732
##   [518,]       733
##   [519,]       735
##   [520,]       736
##   [521,]       737
##   [522,]       741
##   [523,]       742
##   [524,]       745
##   [525,]       746
##   [526,]       748
##   [527,]       750
##   [528,]       751
##   [529,]       752
##   [530,]       753
##   [531,]       754
##   [532,]       755
##   [533,]       758
##   [534,]       759
##   [535,]       760
##   [536,]       761
##   [537,]       762
##   [538,]       763
##   [539,]       764
##   [540,]       765
##   [541,]       766
##   [542,]       767
##   [543,]       771
##   [544,]       772
##   [545,]       778
##   [546,]       781
##   [547,]       784
##   [548,]       785
##   [549,]       787
##   [550,]       788
##   [551,]       789
##   [552,]       790
##   [553,]       791
##   [554,]       792
##   [555,]       793
##   [556,]       795
##   [557,]       796
##   [558,]       797
##   [559,]       800
##   [560,]       803
##   [561,]       804
##   [562,]       805
##   [563,]       806
##   [564,]       807
##   [565,]       808
##   [566,]       809
##   [567,]       810
##   [568,]       811
##   [569,]       812
##   [570,]       813
##   [571,]       814
##   [572,]       817
##   [573,]       818
##   [574,]       821
##   [575,]       823
##   [576,]       824
##   [577,]       826
##   [578,]       827
##   [579,]       828
##   [580,]       829
##   [581,]       830
##   [582,]       831
##   [583,]       832
##   [584,]       834
##   [585,]       836
##   [586,]       837
##   [587,]       838
##   [588,]       840
##   [589,]       842
##   [590,]       843
##   [591,]       844
##   [592,]       845
##   [593,]       846
##   [594,]       847
##   [595,]       848
##   [596,]       849
##   [597,]       851
##   [598,]       854
##   [599,]       857
##   [600,]       859
##   [601,]       864
##   [602,]       865
##   [603,]       866
##   [604,]       867
##   [605,]       868
##   [606,]       870
##   [607,]       872
##   [608,]       876
##   [609,]       877
##   [610,]       878
##   [611,]       880
##   [612,]       881
##   [613,]       882
##   [614,]       883
##   [615,]       887
##   [616,]       888
##   [617,]       889
##   [618,]       892
##   [619,]       893
##   [620,]       894
##   [621,]       895
##   [622,]       896
##   [623,]       897
##   [624,]       898
##   [625,]       900
##   [626,]       901
##   [627,]       903
##   [628,]       904
##   [629,]       906
##   [630,]       907
##   [631,]       908
##   [632,]       909
##   [633,]       910
##   [634,]       911
##   [635,]       912
##   [636,]       914
##   [637,]       915
##   [638,]       916
##   [639,]       917
##   [640,]       918
##   [641,]       919
##   [642,]       920
##   [643,]       922
##   [644,]       924
##   [645,]       926
##   [646,]       927
##   [647,]       929
##   [648,]       930
##   [649,]       931
##   [650,]       932
##   [651,]       933
##   [652,]       934
##   [653,]       935
##   [654,]       936
##   [655,]       937
##   [656,]       938
##   [657,]       942
##   [658,]       943
##   [659,]       944
##   [660,]       945
##   [661,]       946
##   [662,]       947
##   [663,]       948
##   [664,]       949
##   [665,]       950
##   [666,]       951
##   [667,]       952
##   [668,]       953
##   [669,]       954
##   [670,]       957
##   [671,]       958
##   [672,]       959
##   [673,]       961
##   [674,]       963
##   [675,]       964
##   [676,]       970
##   [677,]       971
##   [678,]       974
##   [679,]       975
##   [680,]       976
##   [681,]       978
##   [682,]       980
##   [683,]       981
##   [684,]       982
##   [685,]       983
##   [686,]       984
##   [687,]       985
##   [688,]       986
##   [689,]       988
##   [690,]       989
##   [691,]       991
##   [692,]       992
##   [693,]       993
##   [694,]       994
##   [695,]       995
##   [696,]       996
##   [697,]       997
##   [698,]       998
##   [699,]      1000
##   [700,]      1002
##   [701,]      1003
##   [702,]      1006
##   [703,]      1008
##   [704,]      1009
##   [705,]      1010
##   [706,]      1011
##   [707,]      1012
##   [708,]      1014
##   [709,]      1016
##   [710,]      1017
##   [711,]      1019
##   [712,]      1020
##   [713,]      1021
##   [714,]      1022
##   [715,]      1023
##   [716,]      1024
##   [717,]      1026
##   [718,]      1027
##   [719,]      1029
##   [720,]      1030
##   [721,]      1032
##   [722,]      1033
##   [723,]      1034
##   [724,]      1035
##   [725,]      1038
##   [726,]      1039
##   [727,]      1042
##   [728,]      1043
##   [729,]      1044
##   [730,]      1047
##   [731,]      1048
##   [732,]      1050
##   [733,]      1051
##   [734,]      1052
##   [735,]      1054
##   [736,]      1055
##   [737,]      1056
##   [738,]      1057
##   [739,]      1059
##   [740,]      1060
##   [741,]      1061
##   [742,]      1062
##   [743,]      1063
##   [744,]      1066
##   [745,]      1067
##   [746,]      1068
##   [747,]      1070
##   [748,]      1071
##   [749,]      1072
##   [750,]      1073
##   [751,]      1074
##   [752,]      1077
##   [753,]      1078
##   [754,]      1079
##   [755,]      1081
##   [756,]      1082
##   [757,]      1083
##   [758,]      1085
##   [759,]      1087
##   [760,]      1091
##   [761,]      1092
##   [762,]      1093
##   [763,]      1094
##   [764,]      1095
##   [765,]      1096
##   [766,]      1097
##   [767,]      1098
##   [768,]      1099
##   [769,]      1101
##   [770,]      1104
##   [771,]      1109
##   [772,]      1110
##   [773,]      1112
##   [774,]      1113
##   [775,]      1114
##   [776,]      1117
##   [777,]      1118
##   [778,]      1119
##   [779,]      1120
##   [780,]      1121
##   [781,]      1122
##   [782,]      1123
##   [783,]      1124
##   [784,]      1125
##   [785,]      1129
##   [786,]      1130
##   [787,]      1131
##   [788,]      1132
##   [789,]      1133
##   [790,]      1134
##   [791,]      1135
##   [792,]      1136
##   [793,]      1137
##   [794,]      1139
##   [795,]      1140
##   [796,]      1141
##   [797,]      1142
##   [798,]      1143
##   [799,]      1144
##   [800,]      1146
##   [801,]      1147
##   [802,]      1149
##   [803,]      1150
##   [804,]      1153
##   [805,]      1154
##   [806,]      1155
##   [807,]      1156
##   [808,]      1157
##   [809,]      1158
##   [810,]      1159
##   [811,]      1161
##   [812,]      1162
##   [813,]      1164
##   [814,]      1165
##   [815,]      1166
##   [816,]      1167
##   [817,]      1169
##   [818,]      1170
##   [819,]      1171
##   [820,]      1172
##   [821,]      1173
##   [822,]      1175
##   [823,]      1176
##   [824,]      1177
##   [825,]      1179
##   [826,]      1181
##   [827,]      1185
##   [828,]      1186
##   [829,]      1187
##   [830,]      1188
##   [831,]      1190
##   [832,]      1191
##   [833,]      1192
##   [834,]      1193
##   [835,]      1194
##   [836,]      1195
##   [837,]      1197
##   [838,]      1198
##   [839,]      1199
##   [840,]      1201
##   [841,]      1202
##   [842,]      1203
##   [843,]      1204
##   [844,]      1205
##   [845,]      1208
##   [846,]      1209
##   [847,]      1210
##   [848,]      1211
##   [849,]      1213
##   [850,]      1214
##   [851,]      1215
##   [852,]      1216
##   [853,]      1217
##   [854,]      1219
##   [855,]      1220
##   [856,]      1222
##   [857,]      1224
##   [858,]      1226
##   [859,]      1227
##   [860,]      1228
##   [861,]      1229
##   [862,]      1230
##   [863,]      1231
##   [864,]      1232
##   [865,]      1233
##   [866,]      1234
##   [867,]      1235
##   [868,]      1236
##   [869,]      1237
##   [870,]      1238
##   [871,]      1239
##   [872,]      1240
##   [873,]      1242
##   [874,]      1243
##   [875,]      1245
##   [876,]      1246
##   [877,]      1247
##   [878,]      1250
##   [879,]      1251
##   [880,]      1252
##   [881,]      1255
##   [882,]      1256
##   [883,]      1257
##   [884,]      1258
##   [885,]      1259
##   [886,]      1260
##   [887,]      1261
##   [888,]      1262
##   [889,]      1263
##   [890,]      1265
##   [891,]      1266
##   [892,]      1267
##   [893,]      1268
##   [894,]      1269
##   [895,]      1272
##   [896,]      1274
##   [897,]      1276
##   [898,]      1279
##   [899,]      1281
##   [900,]      1283
##   [901,]      1284
##   [902,]      1285
##   [903,]      1286
##   [904,]      1287
##   [905,]      1288
##   [906,]      1290
##   [907,]      1292
##   [908,]      1293
##   [909,]      1295
##   [910,]      1297
##   [911,]      1298
##   [912,]      1301
##   [913,]      1302
##   [914,]      1303
##   [915,]      1305
##   [916,]      1307
##   [917,]      1308
##   [918,]      1309
##   [919,]      1312
##   [920,]      1315
##   [921,]      1316
##   [922,]      1318
##   [923,]      1319
##   [924,]      1320
##   [925,]      1322
##   [926,]      1323
##   [927,]      1324
##   [928,]      1326
##   [929,]      1327
##   [930,]      1329
##   [931,]      1333
##   [932,]      1334
##   [933,]      1335
##   [934,]      1336
##   [935,]      1337
##   [936,]      1338
##   [937,]      1339
##   [938,]      1340
##   [939,]      1341
##   [940,]      1342
##   [941,]      1343
##   [942,]      1344
##   [943,]      1345
##   [944,]      1346
##   [945,]      1347
##   [946,]      1348
##   [947,]      1351
##   [948,]      1352
##   [949,]      1353
##   [950,]      1356
##   [951,]      1358
##   [952,]      1359
##   [953,]      1360
##   [954,]      1361
##   [955,]      1364
##   [956,]      1365
##   [957,]      1366
##   [958,]      1367
##   [959,]      1369
##   [960,]      1371
##   [961,]      1372
##   [962,]      1375
##   [963,]      1378
##   [964,]      1380
##   [965,]      1381
##   [966,]      1382
##   [967,]      1383
##   [968,]      1384
##   [969,]      1387
##   [970,]      1388
##   [971,]      1389
##   [972,]      1391
##   [973,]      1395
##   [974,]      1396
##   [975,]      1397
##   [976,]      1398
##   [977,]      1400
##   [978,]      1401
##   [979,]      1403
##   [980,]      1404
##   [981,]      1405
##   [982,]      1406
##   [983,]      1409
##   [984,]      1410
##   [985,]      1412
##   [986,]      1413
##   [987,]      1415
##   [988,]      1416
##   [989,]      1417
##   [990,]      1419
##   [991,]      1420
##   [992,]      1421
##   [993,]      1422
##   [994,]      1423
##   [995,]      1424
##   [996,]      1425
##   [997,]      1426
##   [998,]      1427
##   [999,]      1428
##  [1000,]      1429
##  [ reached getOption("max.print") -- omitted 76371 rows ]
```

```r
training <- medical[ Train, ]
testing <- medical[ -Train, ]
model1<- train(No.show ~ Gender+SMS_received, data = training,method = "glm",family="binomial") 
```

```
## Error in na.fail.default(structure(list(No.show = structure(c(1L, 1L, : missing values in object
```

```r
 model1
```

```
## Generalized Linear Model 
## 
## 105 samples
##   3 predictor
##   2 classes: 'No', 'Yes' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 105, 105, 105, 105, 105, 105, ... 
## Resampling results:
## 
##   Accuracy   Kappa     
##   0.7421274  0.02100216
```
---

```r
library(dplyr)
library(ggthemes)
library(ggplot2)
##depending on the age and gender what is the status 
ggplot(data = plots, aes(x = Age, colour = Gender))+
  geom_density()+
  geom_vline(xintercept = 16, linetype = 'longdash')+
  geom_vline(xintercept = 68, linetype = 'longdash')+
  theme_igray()+
  ggtitle('Density of Age by Gender')
```

![plot of chunk unnamed-chunk-5](assets/fig/unnamed-chunk-5-1.png)










   
