nyt <- fread(
    "country	part	full
World	54	42
U.A.E.	99	90
Brunei	90	71
Cuba	89	78
Singapore	89	88
Portugal	88	87
Chile	88	83
Malta	86	86
Cambodia	85	80
Mainland China	85	77
Qatar	83	78
South Korea	81	78
Spain	81	80
Malaysia	80	78
Uruguay	80	76
Canada	80	77
Argentina	80	62
Norway	79	71
Japan	79	76
Iceland	79	77
Aruba	78	73
Denmark	78	76
Italy	78	73
Ireland	78	76
New Zealand	78	70
Australia	78	71
Finland	77	72
Bhutan	77	73
Brazil	77	61
France	77	69
Belgium	76	75
U.K.	76	69
Costa Rica	76	60
Netherlands	76	73
Taiwan	76	44
Maldives	74	68
Ecuador	74	60
Turkmenistan	74	54
Macau	74	57
Sri Lanka	73	63
Bahrain	72	70
Mauritius	72	68
Saudi Arabia	71	65
Luxembourg	71	68
Fiji	71	65
Sweden	71	68
Panama	70	57
Germany	70	68
Mongolia	70	67
Samoa	70	44
Austria	70	65
Israel	69	64
United States	69	59
El Salvador	68	62
Vietnam	68	38
Switzerland	68	66
Morocco	67	62
Turkey	67	60
Iran	67	51
Colombia	66	46
Lithuania	66	63
Thailand	66	54
Latvia	65	59
Curaçao	65	60
Peru	64	51
Greece	64	61
Dominican Rep.	64	51
Hong Kong	63	60
Oman	62	55
New Caledonia	62	57
Estonia	62	59
Hungary	62	59
Tonga	61	38
Czech Republic	60	58
Mexico	59	50
Slovenia	58	54
Belize	58	48
French Polynesia	57	55
India	55	28
Poland	54	53
Cape Verde	54	42
Barbados	53	46
Croatia	51	46
Cyprus	51	48
Azerbaijan	50	45
Guyana	50	34
Tunisia	50	41
Kiribati	49	13
Indonesia	49	32
Venezuela	49	33
Uzbekistan	47	18
Slovakia	47	43
Kazakhstan	46	42
Serbia	46	44
Kosovo	46	41
Trinidad and Tobago	45	45
Timor-Leste	45	30
Laos	45	39
Paraguay	45	36
Suriname	44	37
Russia	43	36
Montenegro	43	40
North Macedonia	42	38
Bolivia	41	34
Honduras	41	37
Jordan	40	36
West Bank & Gaza	40	28
Romania	38	36
Rwanda	38	20
São Tomé and Príncipe	38	13
Bahamas	37	33
Albania	37	33
Pakistan	36	23
Grenada	35	29
Kuwait	35	22
Guatemala	34	22
Botswana	33	16
Moldova	33	34
Belarus	33	24
Comoros	33	23
Bangladesh	32	21
Nepal	31	26
Philippines	30	37
Georgia	30	26
Tajikistan	29	23
Saint Lucia	29	24
Vanuatu	29	12
Ukraine	28	21
South Africa	28	23
Lebanon	27	24
Saint Vincent and the Grenadines	26	19
Myanmar	26	15
Bosnia and Herzegovina	25	22
Zimbabwe	24	19
Libya	24	8
Mauritania	23	15
Eswatini	23	22
Bulgaria	22	24
Jamaica	22	16
Egypt	21	13
Solomon Islands	20	5.3
Armenia	20	8.5
Nicaragua	19	8.6
Angola	18	6.3
Equatorial Guinea	18	14
Iraq	17	10
Kyrgyzstan	16	13
Lesotho	16	16
Guinea-Bissau	16	0.9
Mozambique	16	8.6
Algeria	15	11
Namibia	14	11
Guinea	12	6.1
Togo	12	6
Gambia	10	9.5
Ivory Coast	10	4.4
Afghanistan	9.3	8.4
Uganda	8.7	2.1
Liberia	8	7.5
Republic of the Congo	7.9	2.3
Senegal	7.9	5.4
Ghana	7.7	2.7
Sierra Leone	7.7	3.9
Central African Republic	7.6	6.8
Kenya	7.4	4.1
Gabon	6.8	5
Djibouti	6.8	2.7
Malawi	5.6	3.1
Syria	4.8	4.2
Somalia	3.8	3.7
Ethiopia	3.3	1.2
Nigeria	2.9	1.6
Sudan	2.7	1.4
Benin	2.6	2.2
Niger	2.2	2
Papua New Guinea	2.1	1.2
Yemen	1.8	1.2
Burkina Faso	1.8	1.5
Zambia	1.7	3.5
Cameroon	1.7	0.7
Mali	1.7	1.4
Tanzania	1.5	1.7
Madagascar	1.4	0.7
Chad	1.1	0.4
Haiti	0.9	0.4
South Sudan	0.8	0.6
Congo	0.1	0.1
Burundi	<0.1	<0.1
"
)

nyt[, iso3c := name2code(country)]
dcov <- merge(nyt, fix_countries[date == 2021 & label == "Covid-19",
                                 .(iso3c, value)],
              by = "iso3c", all = T)
dcov <- dcov[!is.na(iso3c)]

df <- merge(dcov,
            pop_disease[year == 2021,
                        .(iso3c,
                          pop_t_le1,
                          poptotal)],
            by = "iso3c")

df[,(c("full", "poptotal")):=
       lapply(.SD, as.numeric), .SDcols = c("full", "poptotal")]
df[,.(nyt_est = weighted_mean(full, poptotal, na.rm = T), 
      owid_est = weighted_mean(value, poptotal, na.rm = T))]
df[is.na(value), .(country, full, poptotal)]

ggplot(dcov, aes(
    x = as.numeric(value),
    y = as.numeric(full),
    label = iso3c
    )) + geom_point() + 
    my_custom_theme + 
    geom_text_repel() + 
    labs(x = "OWID", y = "NYT") + 
    geom_abline(slope = 1, intercept = 0, color = "firebrick")