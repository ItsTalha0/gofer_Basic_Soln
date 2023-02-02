ctype Entity where
      Entity,KeyPerson,Father,Mother,Sibling,Spouse,Child,Relative,Friend :(String,String,String,String,Int) -> Entity



-- The Datatype of Entity is Unique_Key,Name,Gender (Male or Female),Marital Status (Married or Unmarried) and Year of Birth


ctype Acircle where
      FamilyData : [Entity] -> Acircle
      RelativesData : [Entity] -> Acircle
      FriendsData : [Entity] -> Acircle
ctype Circle where
	Network : Entity ->(Acircle,Acircle,Acircle)-> Circle


-------------------------------------------------------- SAMPLE DATA BELOW

ankit 		= KeyPerson.("91908298","Ankit","Male","Married",1973)
father 		= Father.("08782733","Abhijeet","Male","Married",1957)
mother 		= Mother.("8728744","Ketaki","Female","Married",1960)
sister 		= Sibling.("8728745","Sonali","Female","UnMarried",1990)
spouse		= Spouse.("1111112222","Monali","Female","Married",1980)
ankit_family 	= FamilyData.[father,mother,sister,spouse]


himadri 	= Relative.("9190829811","Himadri","Female","UnMarried",1983)
chetan 		= Relative.("128728745","Chetan","Male","Married",1978)
natan		= Relative.("123432155324","Natan","Male","UnMarried",1999)
ankit_relatives = RelativesData.[himadri,chetan,natan]


ajay 		= Friend.("459190829811","Ajay","Male","Married",1981)
rakesh 		= Friend.("459829811","Rakesh","Male","Married",1984)
manpreet 	= Friend.("11111111","Manpreet","Female","Unmarried",1989)
generalZod 	= Friend.("111111112","general Zod","Male","UnMarried",1000)
queenZod 	= Friend.("111111113","queen Zod","Female","UnMarried",1002)
nata		= Friend.("123432155324","Natan","Male","UnMarried",1999)

ankit_friends 	= FriendsData.[ajay,rakesh,generalZod,queenZod,nata]

ankit_circle 	= Network.ankit.(ankit_family,ankit_relatives,ankit_friends)





------------------------- END OF SAMPLE DATA ------------------------------------------



-- On gofer console try finding the type of mycircle,ankit_friends , ankit_relatives, ankit_family and ankit/father/rakesh and see what it gives you


--Question 1

count_circle.(RelativesData.l) 	= length.l
count_circle.(FamilyData.l)	= length.l
count_circle.(FriendsData.l)	= length.l

--Question 2

get_name.(KeyPerson.(a,b,c,d,e)) = b

get_name.(Father.(a,b,c,d,e)) 	= b
get_name.(Mother.(a,b,c,d,e)) 	= b
get_name.(Relative.(a,b,c,d,e)) = b
get_name.(Friend.(a,b,c,d,e))	= b
get_name.(Sibling.(a,b,c,d,e)) 	= b
get_name.(Spouse.(a,b,c,d,e)) 	= b
get_name.(Child.(a,b,c,d,e)) 	= b

people_circle.(RelativesData.l)	= [ get_name.i | i <- l ]
people_circle.(FriendsData.l)	= [ get_name.i | i <- l ]
people_circle.(FamilyData.l)	= [ get_name.i | i <- l ]

--Question 3

id_match.(KeyPerson.(a,b,c,d,e)).id	= id==a
id_match.(Father.(a,b,c,d,e)).id 	= id==a
id_match.(Mother.(a,b,c,d,e)).id 	= id==a
id_match.(Relative.(a,b,c,d,e)).id      = id==a
id_match.(Friend.(a,b,c,d,e)).id	= id==a
id_match.(Sibling.(a,b,c,d,e)).id 	= id==a
id_match.(Spouse.(a,b,c,d,e)).id 	= id==a
id_match.(Child.(a,b,c,d,e)).id 	= id==a


remove_circle.(FamilyData.l).id =RelativesData.([ i | i  <-l,not.(id_match.(i).id)  ])
remove_circle.(RelativesData.l).id =RelativesData.([ i | i  <-l,not.(id_match.(i).id)  ])
remove_circle.(FriendsData.l).id =RelativesData.([ i | i  <-l,not.(id_match.(i).id)  ])

--Question 6


match.(KeyPerson.l)		= l
match.(Father.l)		= l
match.(Mother.l)	 	= l
match.(Relative.l)	        = l
match.(Friend.l)		= l
match.(Sibling.l)	 	= l
match.(Spouse.l)	 	= l
match.(Child.l)			= l

cmp.(a,b,c,d,e).(f,g,h,i,j) 	= a==f && b==g && c==h && d==i && e==j

two_circle.(FamilyData.l).(FriendsData.p) = [ i | i<-l,j<-p, cmp.(match.i).(match.j)  ]
two_circle.(RelativesData.l).(FriendsData.p) = [ i | i<-l,j<-p, cmp.(match.i).(match.j)  ]
two_circle.(FamilyData.l).(RelativesData.p) = [ i | i<-l,j<-p, cmp.(match.i).(match.j)  ]
two_circle.(FriendsData.l).(RelativesData.p) = [ i | i<-l,j<-p, cmp.(match.i).(match.j)  ]
two_circle.(RelativesData.l).(FamilyData.p) = [ i | i<-l,j<-p, cmp.(match.i).(match.j)  ]
two_circle.(FriendsData.l).(FamilyData.p) = [ i | i<-l,j<-p, cmp.(match.i).(match.j)  ]

-- Question 7

compatible.(a,b,c,d,e).(q,w,u,r,t).i = (c /= u) && (abs.(t-e)  <= i ) && (d == "UnMarried") && (r=="UnMarried")

match_couple_cirlce.(FamilyData.l).(FriendsData.p).age = [ (i,j) | i<-l,j<-p, compatible.(match.i).(match.j).age  ]
match_couple_cirlce.(RelativesData.l).(FriendsData.p).age = [ (i,j) | i<-l,j<-p, compatible.(match.i).(match.j).age  ]
match_couple_cirlce.(FamilyData.l).(RelativesData.p).age = [ (i,j) | i<-l,j<-p, compatible.(match.i).(match.j).age  ]
match_couple_cirlce.(FriendsData.l).(RelativesData.p).age = [ (i,j) | i<-l,j<-p, compatible.(match.i).(match.j).age  ]
match_couple_cirlce.(RelativesData.l).(FamilyData.p).age = [ (i,j) | i<-l,j<-p, compatible.(match.i).(match.j).age  ]
match_couple_cirlce.(FriendsData.l).(FamilyData.p).age = [ (i,j) | i<-l,j<-p, compatible.(match.i).(match.j).age  ]

