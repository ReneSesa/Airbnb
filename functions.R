##well written functions

#broji koliko svaka kategorija nekog vektora sadrzi numerickih vrijednosti
count_Numeric_Variables_Per_Category<-function(number_of_numeric_variables, number_of_categories, categorical_variable,
                                              categories_vector,number_of_elements_per_category ) {
  
  for (i in 1:number_of_numeric_variables) {
    for(j in 1:number_of_categories) {

      if(categorical_variable[i]==categories_vector[j]) {
        
        number_of_elements_per_category[j]=number_of_elements_per_category[j]+1
        
      }
      
    }
    
  }
  number_of_elements_per_category
}




#sortira numericke vrijednosti po kategoriji u matricu
add_Numerics_To_Matrix_By_Category<-function(number_of_categories, number_of_elements_per_category,number_of_numeric_variables
                               ,categorical_variable,categories_vector, numeric_variable) {
  
  categorical_numeric_matrix=matrix(0, nrow=number_of_categories, ncol=max(number_of_elements_per_category))
  
  number_of_elements_per_category=rep(0, number_of_categories)
  
  
  for (i in 1:number_of_numeric_variables) {
    for(j in 1:number_of_categories) {

      if(categorical_variable[i]==categories_vector[j]) {
        number_of_elements_per_category[j]=number_of_elements_per_category[j]+1
        categorical_numeric_matrix[j, number_of_elements_per_category[j]]=numeric_variable[i]
        
      }
      
    }
  }
  categorical_numeric_matrix
}





#prima matricu razvrstanih numerickih varijabli po kategorijama
#da bi radila treba joj broj kategorija, broj elemenata po kategoriji i
#vektor koji sadrzi sve kategorije
convert_Matrix_To_List<-function(number_of_categories, categorical_numeric_matrix,
                                 number_of_elements_per_category, categories_vector) {
  
  list_of_categories_filled_with_numerical_values=list()
  
  for (i in 1:number_of_categories) {
    list_of_categories_filled_with_numerical_values[[i]]=c(categorical_numeric_matrix[i,1:number_of_elements_per_category[i]])
  }
  names(list_of_categories_filled_with_numerical_values) <- categories_vector
  
  
  list_of_categories_filled_with_numerical_values
}


#prima numericki i kategorijski vektor
#vraca listu gdje je svaki clan jedna kategorija
#koja je ispunjena svim vrijednostima numericke varijable
sort_Numeric_Categorical<-function(numeric_variable, categorical_variable)
{
  
  number_of_numeric_variables=length(numeric_variable)
  categories_vector=unique(categorical_variable)
  number_of_categories=length(categories_vector)
  number_of_elements_per_category=rep(0, number_of_categories)
  
  
  number_of_elements_per_category=count_Numeric_Variables_Per_Category(number_of_numeric_variables, number_of_categories, categorical_variable,
                                                                      categories_vector,number_of_elements_per_category  )
  
  
  categorical_numeric_matrix=add_Numerics_To_Matrix_By_Category(number_of_categories, number_of_elements_per_category,number_of_numeric_variables
                                                                ,categorical_variable,categories_vector, numeric_variable)
  
  convert_Matrix_To_List(number_of_categories, categorical_numeric_matrix, number_of_elements_per_category, categories_vector)
}




#prima 2 numericka vektora(numericki vektor po kojem se zeli sortirati je nazvan categorical_variable)
#vraca listu gdje je svaki clan jedna kategorija, kategorije su sortirane rastuci buduci da 
#ovdje kategoriju predstavlja neki broj necega 
#svaka kategorija(broj) je ispunjena svim vrijednostima numericke varijable
sort_Numeric_Numeric<-function(numeric_variable, categorical_variable)
{
  
  number_of_numeric_variables=length(numeric_variable)
  categories_vector=sort(unique(categorical_variable))
  number_of_categories=length(categories_vector)
  number_of_elements_per_category=rep(0, number_of_categories)
  
  
  number_of_elements_per_category=count_Numeric_Variables_Per_Category(number_of_numeric_variables, number_of_categories, categorical_variable,
                                                                       categories_vector,number_of_elements_per_category  )
  
  
  categorical_numeric_matrix=add_Numerics_To_Matrix_By_Category(number_of_categories, number_of_elements_per_category,number_of_numeric_variables
                                                                ,categorical_variable,categories_vector, numeric_variable)
  
  convert_Matrix_To_List(number_of_categories, categorical_numeric_matrix, number_of_elements_per_category, categories_vector)
}


#prima dataframe i dodaje mu id column koji je ipunjen rednim brojem stupca
get_Dataframe_With_Id_Row<-function(dataframe) {
  number_of_rows=nrow(dataframe)
  id=c()
  id_len=0
  for (i in 1:number_of_rows) {
    id_len=id_len+1
    id[id_len]=i
  }
  dataframe=cbind(id , dataframe) 
  
  dataframe
}


#prima dataframe i njegov id column (dalo se napisati i da prima samo dataframe, ali eto..)
#zatim vraca vector koji sadrzi id svakog reda kojem je amenities prazan
get_positions_of_empty_amenities<-function(vector, id_vector) {
  len=length(vector)
  counter=0
  vector_of_positions=c()
  vector_of_positions_counter=0
  for(i in 1:len) {
    if(vector[i]=="{}") {
      counter=counter+1
      vector_of_positions_counter=vector_of_positions_counter+1
      vector_of_positions[vector_of_positions_counter]=id_vector[i]
    }
  }
  vector_of_positions
}

#broji koliko je redova kojima je amenities stupac prazan
get_num_of_empty_amenities<-function(vector) {
  len=length(vector)
  counter=0
  for(i in 1:len) {
    if(vector[i]=="{}") {
      counter=counter+1
    }
  }
  counter
}


#prima dataframe i vector koji sadrzi pozicije gdje je amenities string prazan,
#zatim sve redove gdje je amenities prazan mice iz output dataframea
clean_Empty_Amenities<-function(dataframe, vector_of_empty_amenities) {
  clean_data_frame=data.frame()
  number_of_rows=nrow(dataframe)
  num_of_empty_amenities=length(vector_of_empty_amenities)
  
  first_empty=vector_of_empty_amenities[1]
  last_empty=vector_of_empty_amenities[num_of_empty_amenities]
  
  current_interval=c()
  
  clean_data_frame <-  rbind(clean_data_frame , dataframe[c(1:(first_empty-1)),])
  
  
  for (i in 1:(num_of_empty_amenities-1)) {
    current_interval[1]=vector_of_empty_amenities[i]+1
    current_interval[2]=vector_of_empty_amenities[i+1]-1

    if(!(abs(vector_of_empty_amenities[i]-vector_of_empty_amenities[i+1])==1)) {
    clean_data_frame <-  rbind(clean_data_frame , dataframe[current_interval[1]:current_interval[2],]) 
    }
  }
  
  
  clean_data_frame <-  rbind(clean_data_frame , dataframe[c((last_empty+1):(number_of_rows)),])
  clean_data_frame
}




#prima jedan amenities string i od njega radi vector odvojenih amenitiesa
make_Vector_Of_Amentities<-function(amentities_string) {
  l=list()
  amentities_string=gsub("[{}]", "", amentities_string)
  l=strsplit(amentities_string, ",")
  l[[1]]=gsub("[^[:alnum:]]", "", l[[1]])
  vector_of_amentites=c(l[[1]])
  vector_of_amentites
  
}


#vraca vector id-eva kuca u nyc
get_positions_of_nyc_houses<-function(vector, id_vector) {
  len=length(vector)
  counter=0
  vector_of_positions=c()
  vector_of_positions_counter=0
  for(i in 1:len) {
    if(vector[i]=="House") {
      counter=counter+1
      vector_of_positions_counter=vector_of_positions_counter+1
      vector_of_positions[vector_of_positions_counter]=id_vector[i]
    }
  }
  vector_of_positions
}
##nadi outliere


  


####poorly written functions

#so far so good :)
    
get_DC<-function(vector, id_vector) {
  len=length(vector)
  counter=0
  vector_of_positions=c()
  vector_of_positions_counter=0
  for(i in 1:len) {
    if(vector[i]=="DC") {
      counter=counter+1
      vector_of_positions_counter=vector_of_positions_counter+1
      vector_of_positions[vector_of_positions_counter]=id_vector[i]
    }
  }
  vector_of_positions
}

