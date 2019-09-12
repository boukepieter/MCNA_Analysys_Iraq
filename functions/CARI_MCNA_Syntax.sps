* Encoding: UTF-8.
* Encoding: .
**************************************************************************
**   Name of Survey :Syrian Refugee Joint Vulnerability Assessment Country Iraq
**   CARI calculations: FCS, exp share and CSI
**   Prepared by Saman Ahmed
**   Date 17 April 2018  
*************************************************************************** 

**********************  1 . FCS  ************************** 
  Step 1. Calculate FCS
************************************************************ 
food_securityfcscereals            How many days were Cereals, grains, roots and tubers: rice, pasta, bread, potato consumed in the last 7 days?
food_securityfcsnuts_seed     	How many days were Legumes / nuts : beans, peanuts, lentils, nut, soy, and / or other nuts consumed in the last 7 days?
food_securityfcsmilk_dairy              	How many days were Milk and other dairy products: fresh milk / sour, yogurt, cheese, other dairy products consumed in the last 7 days?
food_securityfcsmeat      Meat, fish and eggs: goat, beef, chicken, , fish, including canned tuna, and / or other seafood, eggs (meat and fish consumed in large quantities and not as a condiment)
food_securityfcsvegetables    	Vegetables and leaves: spinach, onion, tomatoes, carrots, peppers, green beans, lettuce, cabbages, egg plants, etc
food_securityfcsfruits   Fruits: banana, apple, lemon, mango, watermelon, apricot, peach, pineapple, passion, gishta, orange, avocado, wild fruits etc
consumption_detailsorange_fruits_cons   	How many days were Orange fruits (Fruits rich in Vitamin A): mango,  apricot, peach, guava, orange consumed in the last 7 days?
food_securityfcsoil_fats     How many days were Oil / fat / butter: vegetable oil, palm oil, margarine, other fats / oil consumed in the last 7 days?
food_securityfcssweets          How many days were Sugar, or sweet: sugar, honey, jam, cakes, candy, cookies, pastries, cakes and other sweet (sugary drinks) consumed in the last 7 days?


** Check that there are no missing values (freq). 

**  then make the groups.
COMPUTE staples =food_securityfcscereals .
COMPUTE pulses =  food_securityfcsnuts_seed. 
COMPUTE dairy = food_securityfcsmilk_dairy.
COMPUTE meat = food_securityfcsmeat  .
COMPUTE vegetables = food_securityfcsvegetables.
COMPUTE fruits = food_securityfcsfruits .
COMPUTE oil = food_securityfcsoil_fats.
COMPUTE sugar =  food_securityfcssweets.
EXECUTE.
  
***
****COMPUTE fruits = consumption_detailsfruits_cons.

**  Max value per group should be 7. Here we recode values above 7 days to 7.

DATASET ACTIVATE DataSet1.
RECODE food_securityfcscereals food_securityfcsnuts_seed food_securityfcsmilk_dairy 
    food_securityfcsmeat food_securityfcsvegetables food_securityfcsfruits food_securityfcsoil_fats 
    food_securityfcssweets food_securityfcsspices_condiments (7 thru Highest=7).
EXECUTE.

RECODE staples  pulses  meat  vegetables  fruits  dairy  oil  sugar  (7 THRU HIGHEST=7).  
EXECUTE .

**  create the score, following the FCS guidelines. 
COMPUTE FCS = sum((2 * staples), (3*pulses), (4*meat), vegetables, fruits, (4*dairy), (0.5*oil), (0.5*sugar)).
EXECUTE.

*** create FC groups.  
*with suger and oil, threshold use is 28/42. 
RECODE
  FCS  (Lowest thru 28=1)  (28.5 thru 42=2)  (42.5 thru Highest=3)  INTO  FC_groups .
EXECUTE .

* Add variable name, and the labels. 
VARIABLE LABELS FC_groups 'Food Consumption Score categories'.
FORMAT FC_groups (F5.0).
VALUE LABELS FC_groups
        1  'Poor'
        2  'Border line'
        3  'Acceptable'.
MISSING VALUES FC_groups ( ).
VARIABLE LEVEL FC_groups ( NOMINAL).
EXECUTE.
***************************** CARI:    Recode the food consumption groups into the 4-point scale****************************************.
***These categories recoded into FOUR categoreis, and the  REVERSE order from FCS: acceptable (=1), borderline (=3) and poor (=4).

RECODE FC_groups (1=4) (2=3) (3=1) INTO FCS_4pt. 
VARIABLE LABELS FCS_4pt 'Food Consumption group'. 
EXECUTE.

VALUE LABELS FCS_4pt 
  1 'Food secure'
  2 'Marginally food secure'
  3 'Moderately food insecure'
  4 'Severely food insecure'.
EXECUTE.

FREQUENCIES VARIABLES=FCS_4pt 
/ORDER=ANALYSIS. 


********************************  COPING  **********************************************************
The list of livelihood based coping questions from the questionnare is included here to make it easier to remember the codes:
food_securitycoping_strategies_food2selling_assets            1. Livelihood-based Coping: Selling household properties (refrigerator, television, jewelry…)                                 (stress)
food_securitycoping_strategies_food2spent_savings            2. Livelihood-based Coping: Spending the savings                                                                                           (stress)
food_securitycoping_strategies_food2borrow_debt              3. Livelihood-based Coping: Buying food on credit or through borrowed money from relatives and friends               (stress)
food_securitycoping_strategies_food2selling_transportation_means         4. Livelihood-based Coping: Selling means of transport (car, motorbike)                                                             (crisis)
food_securitycoping_strategies_food2child_droput_school                  5. Livelihood-based Coping: Children dropout from school                                                                                 (emergency)
food_securitycoping_strategies_food2reduce_spending         6. Livelihood-based Coping: Reducing expenditure on non-food items (health, education)                                     (stress)
food_securitycoping_strategies_food2change_place        7. Livelihood-based Coping: Changing place of residence and accommodation to reduce expenses                      (crisis)
food_securitycoping_strategies_food2male_illigal_acts        8. Livelihood-based Coping: Accepting that adult males of the family are engaged in illegal acts and risks             *(emergency)
food_securitycoping_strategies_food2female_illigal_acts       9. Livelihood-based Coping: Accepting that adult females of the family are engaged in illegal acts and risks          *(emergency)
food_securitycoping_strategies_food2child_work             10. Livelihood-based Coping: Children under 18 work to provide resources                                                            (crisis)
food_securitycoping_strategies_food2family_migrating        11. Livelihood-based Coping: Whole family are migrating                                                                                    (emergency)
food_securitycoping_strategies_food2social_events  12. Livelihood-based Coping:  Attending banquets held on religious and social events to have food                         (emergency)
food_securitycoping_strategies_food2child_marriage        13. Livelihood-based Coping: Child marriage                                                                                                      *(emergency) 
food_securitycoping_strategies_food2forced_marriage          14. Livelihood-based Coping: Forced marriage (for adults)                                                                                   *(emergency)

******filtering coping strategies*****.
* code into binary to say 1 if used (now or in the past) and 0 if never used or not applicable.

IF  (food_securitycoping_strategies_food2selling_assets=1  | food_securitycoping_strategies_food2selling_assets=3) SellingAsset1=1.
IF  (food_securitycoping_strategies_food2spent_savings=1  | food_securitycoping_strategies_food2spent_savings=3) SpentSaving1=1.
IF  (food_securitycoping_strategies_food2borrow_debt=1  | food_securitycoping_strategies_food2borrow_debt=3) Buyingfood_credit1=1.
IF  (food_securitycoping_strategies_food2selling_transportation_means=1  | food_securitycoping_strategies_food2selling_transportation_means=3) SellMeans_trans1=1.
IF  (food_securitycoping_strategies_food2child_droput_school=1  | food_securitycoping_strategies_food2child_droput_school=3) ChildDropOut1=1.
IF  (food_securitycoping_strategies_food2reduce_spending=1  | food_securitycoping_strategies_food2reduce_spending=3) RedNonfoodEx1=1.
IF  (food_securitycoping_strategies_food2change_place=1  | food_securitycoping_strategies_food2change_place=3) ChangingRes1=1.
IF  (food_securitycoping_strategies_food2male_illigal_acts=1  |food_securitycoping_strategies_food2male_illigal_acts=3) Male_accp_risk1=1.
IF  (food_securitycoping_strategies_food2female_illigal_acts=1  | food_securitycoping_strategies_food2female_illigal_acts=3) Female_accp_risk1=1.
IF  (food_securitycoping_strategies_food2child_work=1  | food_securitycoping_strategies_food2child_work=3) ChildWork1=1.
IF  (food_securitycoping_strategies_food2family_migrating=1  | food_securitycoping_strategies_food2family_migrating=3) WholeFamMigrated1=1.
IF  (food_securitycoping_strategies_food2social_events=1  | food_securitycoping_strategies_food2social_events =3) AttndSocialEv1=1.
IF  (food_securitycoping_strategies_food2child_marriage=1  | food_securitycoping_strategies_food2child_marriage=3) ChildMarriage1=1.
IF  (food_securitycoping_strategies_food2forced_marriage=1  |food_securitycoping_strategies_food2forced_marriage=3) ForcedMarriage1=1.
EXECUTE.

RECODE SellingAsset1 SpentSaving1 Buyingfood_credit1 SellMeans_trans1 ChildDropOut1 RedNonfoodEx1 ChangingRes1 Male_accp_risk1 Female_accp_risk1 
 ChildWork1 WholeFamMigrated1 AttndSocialEv1 ChildMarriage1 ForcedMarriage1 (SYSMIS=0).
EXECUTE.

***** Coping Strategy (Stress, Crisis, Emergency)******

**Stress Strategies** (1 2 3 6).
COMPUTE Stress_Coping=0.
IF  (SellingAsset1 = 1 | SpentSaving1 = 1 | Buyingfood_credit1 = 1 | RedNonfoodEx1 = 1) 
    Stress_Coping=2.
EXECUTE.

**Crisis Strategies** (4  7 10  ) .
COMPUTE Crisis_Coping=0.
IF  (SellMeans_trans1=1 | ChangingRes1 = 1 | ChildWork1 = 1) Crisis_Coping=3.
EXECUTE.

**Emergency Coping*** ( 5 8  9  11 12 13  14 ) .
COMPUTE emergency_Coping = 0.
IF  (ChildDropOut1 = 1 | Male_accp_risk1=1 | Female_accp_risk1=1 |  WholeFamMigrated1= 1 | AttndSocialEv1 | ChildMarriage1=1 | ForcedMarriage1=1) Emergency_Coping=4.
EXECUTE.

* Define Variable Properties.
VARIABLE LABELS  stress_coping 'did HH engage in stress coping strategies?'.
VARIABLE LABELS  crisis_coping 'did HH engage in crisis coping strategies?'.
VARIABLE LABELS  emergency_coping 'did HH engage in emergency coping strategies?'.
EXECUTE.

* Chose the highets - max - coping used. 
COMPUTE Max_coping_behavior=MAX(stress_Coping, crisis_Coping, emergency_Coping).
EXECUTE.

* if no coping is used, recode to 1. 
RECODE  Max_coping_behavior (0=1).
EXECUTE.

VALUE LABELS Max_coping_behavior 1 'HH not adopting coping strategies' 2 'Stress Coping Strategies' 3 'Crisis Coping Strategies' 4 'Emergency Coping Strategies'.
EXECUTE.


COMPUTE Max_coping_4pt = Max_coping_behavior.
VARIABLE LABELS Max_coping_4pt 'Livelihood coping strategy indicators'.
VALUE LABELS Max_coping_4pt
  1 'HH not adopting coping strategies'
  2 'Stress Coping Strategies'
  3 'Crisis Coping Strategies'
  4 'Emergency Coping Strategies'.
EXECUTE.

FREQUENCIES VARIABLES=Max_coping_4pt 
/ORDER=ANALYSIS. 

**************************************************    Food expenditure share   ***************************************************************************:
  Step 2. Calculate Food expenditure share 
********************************************************************************************************* 

********Food Expenditure ************
livelihoodsexpensesfood_exp_basic_needs 
********Non-Food Expenditure ************
livelihoodsexpensesrent_exp_basic_needs                                   House rent expenditure
livelihoodsexpensesshelter_exp_basic_needs                                Shelter materials/ maintenance repairs expenditure    
livelihoodsexpenseselectric_exp_basic_needs                              Electricity expenditure    
livelihoodsexpensesmedical_exp_basic_needs                                 Health and medical costs expenditure
livelihoodsexpenseseducation_exp_basic_needs                        Education expenditure
livelihoodsexpenseswater_exp_basic_needs                                 Water expenditure
livelihoodsexpensesNFI_exp_basic_needs                                  Other nonfood items
livelihoodsexpensestransportation_exp_basic_needs                    Transportation
livelihoodsexpensescommunication_exp_basic_needs              Telecommunications (mobile, internet, satellite) expenditure
livelihoodsexpensesdebt_exp_basic_needs                                Debts
livelihoodsexpensesproductive_assets                                   fuel/gas
livelihoodsexpensesother_payment_exp_basic_needs                                  All the rest of expenditures




COMPUTE Total_HH_M_Exp=SUM(livelihoodsexpensesrent_exp_basic_needs,livelihoodsexpensesshelter_exp_basic_needs,livelihoodsexpensesfood_exp_basic_needs,
livelihoodsexpenseselectric_exp_basic_needs,livelihoodsexpensesmedical_exp_basic_needs,livelihoodsexpenseseducation_exp_basic_needs,livelihoodsexpenseswater_exp_basic_needs
,livelihoodsexpensesNFI_exp_basic_needs,livelihoodsexpensestransportation_exp_basic_needs,livelihoodsexpensescommunication_exp_basic_needs,livelihoodsexpensesdebt_exp_basic_needs,
livelihoodsexpensesproductive_assets,livelihoodsexpensesother_payment_exp_basic_needs).
VARIABLE LABELS  Total_HH_M_Exp 'Total Household Monthly Expenditure'.
EXECUTE.


COMPUTE FoodExp_share=(livelihoodsexpensesfood_exp_basic_needs/Total_HH_M_Exp)*100.
EXECUTE.

VARIABLE LABELS FoodExp_share 'Share of Household Monthly Expenditure on Food from the total Expenditure'.
EXECUTE.


*** Make food expenditure share groups.
RECODE FoodExp_share (Lowest thru 50=1) (50 thru 65=2) (65 thru 75=3) (75 thru Highest=4)
INTO Foodexp_4pt.
VARIABLE LABELS Foodexp_4pt 'Food expenditure share categories'.
EXECUTE.

VALUE LABELS Foodexp_4pt
  1.00 'Low ' 
  2.00 'Medium ' 
  3.00 'High' 
  4.00 'Very High'.
EXECUTE.
 
FREQUENCIES VARIABLES=Foodexp_4pt 
/ORDER=ANALYSIS. 



          ********* Food Security Classification******

COMPUTE Mean_coping_capacity=MEAN(Max_coping_4pt,Foodexp_4pt).
EXECUTE.

COMPUTE FS_class_unrounded=MEAN(FCS_4pt,Mean_coping_capacity).
EXECUTE.

COMPUTE FS_Final=RND(FS_class_unrounded).
EXECUTE.

* Define Variable Properties.
*FS_Final.
VARIABLE LABELS  FS_Final 'Food Security Index'.
VALUE LABELS FS_Final
  1 'Food secure'
  2 'Marginally food secure'
  3 'Moderately food insecure'
  4 'Severely food insecure'.
EXECUTE.

RECODE FS_Final (1=1) (2=2) (3=3) (4=3) (SYSMIS=SYSMIS) (MISSING=SYSMIS) INTO FS_3pt.
EXECUTE.

VARIABLE LABELS  FS_3pt 'Food Security Index (3 point)'.
VALUE LABELS FS_3pt
  1 'Food secure'
  2 'Vulnerable to food insecurity'
  3 'Food insecure'.
EXECUTE.

FREQUENCIES VARIABLES=FS_Final FS_3pt
  /ORDER=ANALYSIS.





