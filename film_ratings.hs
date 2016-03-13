-- This is a small program which mocks a film database website. A user can add a film, rate them, as well as 
-- display them or various data about multiple films in the database file. 
import Data.Char
import Data.List
import Text.Printf

-- Types
-- =====
-- Here, I am defining the data types I will be using throughout the program
type User = String
type Score = Int

type Title = String
type Director = String
type Year = Int
type Rating = (User, Score)
type Ratings = [Rating]

type Film = (Title, Director, Year, Ratings)
type RatedFilm = (Float, Title, Director, Year, Ratings)
type Database = [Film]

--This function is used for the demo functions. 
--Returns: The entire list of films, as provided on moodle, formatted to match the Film data type 
testDatabase :: Database
testDatabase = [("Blade Runner", "Ridley Scott", 1982, [("Amy",5), ("Bill",8), ("Ian",7), ("Kevin",9), ("Emma",4), ("Sam",7), ("Megan",4)]),("The Fly", "David Cronenberg", 1986, [("Megan",4), ("Fred",7), ("Chris",5), ("Ian",0), ("Amy",6)]),("Psycho", "Alfred Hitchcock", 1960, [("Bill",4), ("Jo",4), ("Garry",8), ("Kevin",7), ("Olga",8), ("Liz",10), ("Ian",9)]), ("Body Of Lies", "Ridley Scott", 2008, [("Sam",3), ("Neal",7), ("Kevin",2), ("Chris",5), ("Olga",6)]),("Avatar", "James Cameron", 2009, [("Olga",1), ("Wally",8), ("Megan",9), ("Tim",5), ("Zoe",8), ("Emma",3)]),("Titanic", "James Cameron", 1997, [("Zoe",7), ("Amy",1), ("Emma",5), ("Heidi",3), ("Jo",8), ("Megan",5), ("Olga",7), ("Tim",10)]), ("The Departed", "Martin Scorsese", 2006, [("Heidi",3), ("Jo",8), ("Megan",5), ("Tim",3), ("Fred",5)]),("Aliens", "Ridley Scott", 1986, [("Fred",9), ("Dave",6), ("Amy",10), ("Bill",7), ("Wally",1), ("Zoe",5)]), ("Kingdom Of Heaven", "Ridley Scott", 2005, [("Garry",3), ("Chris",7), ("Emma",5), ("Bill",1), ("Dave",3)]), ("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, [("Ian",9), ("Amy",1), ("Emma",7), ("Sam",8), ("Wally",5), ("Zoe",6)]), ("Bridge of Spies", "Steven Spielberg", 2015, [("Fred",3), ("Garry",4), ("Amy",10), ("Bill",7), ("Wally",6)]),("Vertigo", "Alfred Hitchcock", 1958, [("Bill",8), ("Emma",5), ("Garry",1), ("Kevin",6), ("Olga",6), ("Tim",10)]),("The Birds", "Alfred Hitchcock", 1963, [("Garry",7), ("Kevin",8), ("Olga",4), ("Tim",8), ("Wally",3)]),("Jaws", "Steven Spielberg", 1975, [("Fred",3), ("Garry",0), ("Jo",3), ("Neal",9), ("Emma",7)]),("The Martian", "Ridley Scott", 2015, [("Emma",7), ("Sam",8), ("Wally",5), ("Dave",10)]),("The Shawshank Redemption", "Frank Darabont", 1994, [("Jo",8), ("Sam",10), ("Zoe",4), ("Dave",7), ("Emma",3), ("Garry",10), ("Kevin",7)]),("Gladiator", "Ridley Scott", 2000, [("Garry",7), ("Ian",4), ("Neal",5), ("Wally",3), ("Emma",4)]),("The Green Mile", "Frank Darabont", 1999, [("Sam",3), ("Zoe",4), ("Dave",7), ("Wally",5), ("Jo",5)]),("True Lies", "James Cameron", 1994, [("Dave",3), ("Kevin",10), ("Jo",0)]),("Super 8", "J J Abrams", 2011, [("Dave",7), ("Wally",3), ("Garry",5), ("Megan",4) ]),("Minority Report", "Steven Spielberg", 2002, [("Dave",6), ("Garry",6), ("Megan",2), ("Sam",7), ("Wally",8)]),("War Horse", "Steven Spielberg", 2011, [("Dave",6), ("Garry",6), ("Megan",3), ("Sam",7), ("Wally",8), ("Zoe",8)]),("The Terminal", "Steven Spielberg", 2004, [("Olga",8), ("Heidi",8), ("Bill",2), ("Sam",6), ("Garry",8)]),("Star Wars: The Force Awakens", "J J Abrams", 2015, [("Olga",6), ("Zoe",6), ("Bill",9), ("Sam",7), ("Wally",8), ("Emma",8)]),("Hugo", "Martin Scorsese", 2011, [("Sam",9), ("Wally",3), ("Zoe",5), ("Liz",7)])]

--  Functional Code
--  ===============

--i) This function adds a film to a given database 
--Takes: Title (the name of the film), Director (the name of the director), Year (the year for the film), Database (the database to append the new film to)
--Returns: The database it was given, with the new film data appended in the correct format
addFilm :: Title -> Director -> Year -> Database -> Database
addFilm title director year database = database ++ [(title, director, year, [])]

--ii) This function prints out all the films in a given database
--Takes: Database (a database of films)
--Outputs: The formatted string of all films returned by the filmsAsString function
getAllFilms :: Database -> IO ()
getAllFilms database = putStrLn (filmsAsString database)

--iii) This function returns a list of all the films directed by a certain director
--Takes: Database (the database of films to query), Director (the name query of the director)
--Returns: A list of films with a director that matches the query
getFilmsByDirector :: Database -> Director -> Database
getFilmsByDirector database directorQuery = [(title, director, year, ratings) | 
                                            (title, director, year, ratings) <- database, director == directorQuery]

--iv) This function returns a formatted string of all the films with a website rating of 7 or more
--Takes: Database (the database of films to query)
--Returns: A formatted string of films
getFilmsWithRatingSevenPlus :: Database -> String
getFilmsWithRatingSevenPlus database = filmsAsString [(title, director, year, ratings) | 
                                       (title, director, year, ratings) <- database, getAverageRating ratings >= 7]

--v) This function returns the average rating from a list of films directed by one person
-- Foldr is used to add together the total values of the website ratings, which is then divided by the number of films from this director
--Takes: Database (the database of films to query) , Director (the name of the director)
--Returns: An average float value for the ratings
getDirectorAverageRating :: Database -> Director -> Float
getDirectorAverageRating database director = (foldr (+) 0 [getAverageRating ratings | 
                                             (_, _, _, ratings) <- (getFilmsByDirector database director)]) / (fromIntegral (length (getFilmsByDirector database director)))

--vi) This function returns a list of ratings that a user has given to a database
-- A list of Rating type is compiled if the ratings for a film contain a rating from a user with the given name
--Takes: Database (the database of films to query), User (the username to check a film's ratings for)
--Returns: a Ratings type containing all the results
hasARatingFrom :: Database -> User -> Ratings
hasARatingFrom films user = [(title, userRatingScore ratings user) | 
                            (title, _, _, ratings) <- films, userRatingScore ratings user >= 0]

--This is a helper funcion for the hasARatingFrom function.
--Takes: Ratings (a film's list of ratings), User (a username to check the ratings for)
--Returns: A score that the user gave that film, or -1, if there is no rating from that user for the film
userRatingScore :: Ratings -> User -> Score
userRatingScore [] user = -1 
userRatingScore ((name, score):rest) user = if user==name then score else userRatingScore rest user

--vii) This function is used to add a rating to a film. It calls the applyRating function to each film in the database and returns the result
--Takes: Database (the database to rate a film in), Title (film title), User (username), Score (user's score for the film)
--Returns: the database, with the rating applied to the correct place
rateFilm :: Database -> Title -> User -> Score -> Database
rateFilm films title user score = [applyRating film title user score | film <- films]

--This is a helper function for rateFilm. Given a film, it checks first if this is the film that the user wishes to rate. If it is not, it returns the film unchanged.
--If it is, however, it then checks if there is already a rating from the user, using the hasARatingFrom function. If it does, it calls the updateRating function with the data and returns its result.
--Otherwise, it appends the user and score to the review list for the film, and returns the results
--Takes: Film (a singular film), Title (film title to rate), User (username to rate), Score (user's rating score)
--Returns: The resulting, processed film
applyRating :: Film -> Title -> User -> Score -> Film
applyRating (filmTitle, director, year , reviews) title user score
            | filmTitle /= title                                                  = (filmTitle, director, year , reviews)
            | [(filmTitle, director, year , reviews)] `hasARatingFrom` user /= [] = (filmTitle, director, year , updateRating reviews user score) 
            | otherwise                                                           = (filmTitle, director, year , reviews ++ [(user, score)])

--This is a helper function for applyRating. Given a film's list of ratings and a new rating, it recursively checks if each rating's username matches the new rating's username, and replaces it
--Takes: Ratings (a list of ratings), User (username for new rating), Score (score for new rating)
--Returns: new list of ratings
updateRating :: Ratings -> User -> Score -> Ratings
updateRating [] user score = []
updateRating ((ratingName , ratingScore): rest) user score = if ratingName==user then 
                                                                [(ratingName, score)] ++ updateRating rest user score 
                                                            else 
                                                                [(ratingName, ratingScore)] ++ updateRating rest user score

--viii) This function returns a list of films between two given dates, sorted in descending order of website rating. It sorts in descending order the results from the createRatedFilmList function it calls.
--Takes: Database (the database to process), Year (one of the years), Year (the other year)
--Returns: a list of RatedFilm
getFilmsBetween :: Database -> Year -> Year -> [RatedFilm]
getFilmsBetween films y1 y2 = reverse (sort ( filterYears (createRatedFilmList films) y1 y2))

--This is a helper function for the getFilmsBetween function. It first filters the list of rated films to have a release date higher than the smaller of the two given dates, then filters against the larger date.
--Takes: [RatedFilm] (the resulting list of rated films from the createRatedFilmList function it was passed), Year (one of the years), Year (the other year)
--Returns: the filtered list of rated films, with release dates contained between the two years specified
filterYears :: [RatedFilm] -> Year -> Year -> [RatedFilm]
filterYears films y1 y2 = filter (\(websiteRating, title, director, year, ratings) -> year <= (max y1 y2)) (filter (\(websiteRating, title, director, year, ratings) -> year >= (min y1 y2)) films)

--This is a helper function for the getFilmsBetween function. Given a list of films, it recursively prepends the website rating to each film by calling the getAverageRating function on the ratings.
--Takes: Database (the database to process)
--Returns: a list of films with their website rating prepended (RatedFilm type)
createRatedFilmList :: Database -> [RatedFilm]
createRatedFilmList [] = []
createRatedFilmList ((title, director, year, ratings):rest) = [(getAverageRating ratings, title, director, year, ratings)] 
                                                              ++ createRatedFilmList rest


--  Helper Functions
--  ================

--Given a database, this function recursively formats the contents of each Film with individual formatting criteria, and returns the finished result
--Takes: Database (the database to process)
--Returns: A completely formatted string, containing line breaks between the films
filmsAsString :: Database -> String
filmsAsString [] = ""
filmsAsString ((title,director,year,ratings):rest) = printf "%30s" title ++ printf "%25s" director ++ printf "%10s" (show year) ++ printf "%10.2f" (getAverageRating ratings) ++ "\n" ++ filmsAsString rest 

--This function carries out the same task as the filmsAsString function, but for films that contain a website rating
--Takes: Database (the database to process)
--Returns: A completely formatted string, containing line breaks between the films
ratedFilmsAsString :: [RatedFilm] -> String
ratedFilmsAsString [] = ""
ratedFilmsAsString ((websiterating, title,director,year,ratings):rest) = printf "%30s" title ++ printf "%25s" director ++ printf "%10s" (show year) ++ printf "%10.2f" (getAverageRating ratings) ++ "\n" ++ ratedFilmsAsString rest 

--This function calculates the average value of all the scores in a list of ratings.
--The value is caluclated using foldr to add together all the scores, then divide that by the rating counr
--Takes: Ratings (the list of the ratings needed to process)
--Returns: A value of the calculated average
getAverageRating :: Ratings -> Float
getAverageRating [] = 0
getAverageRating ratings = fromIntegral (foldr (+) 0 (getRatings ratings)) /  fromIntegral (length ratings)

--This is a helper function used to return a list of scores from a list of ratings, to allow easier processing
--Takes: Ratings (a list of ratings to process)
--Returns: A list of just the score values
getRatings :: Ratings -> [Int]
getRatings ratings = [rating | (user, rating) <- ratings]

--This helper function recursively checks if a given title is present in a film database
--Takes: Database (the database to process), Title (the title of the film to query)
--Returns: a boolean value indicating the presence of the film
filmExists :: Database -> Title -> Bool
filmExists [] title = False 
filmExists ((filmTitle,_,_,_):rest) title = if filmTitle==title then True else False || filmExists rest title

--This function formats a list of ratings to be displayed properly on the screen 
--Takes: Ratings (a list of ratings to be processed)
--Returns: A formatted string representation of the ratings input
listOfRatingsToString :: Ratings -> String
listOfRatingsToString [] = ""
listOfRatingsToString ((title, score):rest) = printf "%30s" title ++ printf "%5s" (show score) ++ "\n" ++ listOfRatingsToString rest

--This function is used to check wether an integer is within the given range
--Takes: Int (the input to be checked), Int (the smaller range value), Int (the larger range value)
--Returns: a boolean value to represent if the value is within the range
inRange :: Int -> Int -> Int -> Bool
inRange input min max = input >= min && input <= max

--This function simply outputs the error message for the above function, should the check fail. It shows the range, for better UX
--Takes: Int (the minimum range), Int (the maximum range)
--Outputs: the error message, containing the range values
showRangeError :: Int -> Int -> IO ()
showRangeError min max = do
    putStrLn ("Please enter a value between " ++ (show min) ++ " and " ++ (show max) ++ "\n")

--This helper function validates that a string input given represents a numerical value, to avoid runtime errors
--Takes: String (the string input)
--Returns: a boolean value representing if the string is indeed numeric
isNum :: String -> Bool
isNum string = all isDigit string

--This function outputs an error message for when a string is entered instead of an integer 
showNumError :: IO ()
showNumError = putStrLn "Please enter a number\n"

--This function outputs an error message for when an integer is entered instead of a string 
showStringError :: IO ()
showStringError = putStrLn "Please enter words\n"

--This function is used to validate that an input is existent, numeric, and in the provided range
--Takes: String (the input), Int (the minimum range limit), Int (the maximum range limit)
--Returns: an IO Boolean to represent the validity of the input
validateIntAndRange :: String -> Int -> Int -> IO Bool
validateIntAndRange input min max = do

    --Check input is not null
    if input == "" then do
        putStrLn "Please enter a value\n"
        return False
    else do
        --Check input is numeric
        let inputValid = isNum input

        if inputValid then do
            let inputInt = (read input)

            --Check input range
            if(inRange inputInt min max) then do
                return True
            else do
                showRangeError min max
                return False
        else do
            showNumError    
            return False

validateString :: String -> IO Bool
validateString input = do

    if input == "" then do
        putStrLn "Please enter a value\n"
        return False
    else do
        let inputIsNum = isNum input

        if (not inputIsNum) then do
            return True
        else do
            showStringError
            return False

-- Demo function to test basic functionality 
-- =========================================

demo :: Int -> IO ()
-- Show all films after adding the new film
demo 1  = putStrLn (filmsAsString (addFilm "The BFG" "Steven Spielberg" 2016 testDatabase))
-- Show all films in the database
demo 2 = getAllFilms testDatabase
-- Show all films directed by Ridley Scott
demo 3  = putStrLn (filmsAsString (getFilmsByDirector testDatabase "Ridley Scott"))
-- Show all films with a rating of seven or more
demo 4  = putStrLn (getFilmsWithRatingSevenPlus testDatabase)
-- Show average website rating for a director (MAYBE NOT OPTIMAL WAY TO DO THE FORMATTING)
demo 5  = putStrLn (printf "%.4s" (show (getDirectorAverageRating testDatabase "Ridley Scott")))
-- Show all films reviewed by Emma, with corresponding score
demo 6  = putStrLn (listOfRatingsToString (testDatabase `hasARatingFrom` "Emma"))
-- Show all films after Emma rates Hugo 10
demo 7  = putStrLn (filmsAsString (rateFilm testDatabase "Hugo" "Emma" 10))
-- Show all films after Emma rates Avatar 10 (Check that there was no error in question, too similar to demo 7)
demo 77 = putStrLn (filmsAsString (rateFilm testDatabase "Avatar" "Emma" 10))
-- Show all films between 2010 and 2014 sorted by website rating
demo 8  = putStrLn (ratedFilmsAsString (getFilmsBetween testDatabase 2010 2014))

-- User Interface
-- ==============

-- Asks for username, calls the database reading and then the main menu with these values
main :: IO ()
main = do
    username <- askUserName
    database <- readFilmFile
    showMainMenu database username
    return ()

-- This function reads the database file and returns the contents as a list of Film
-- Returns: an IO Film list value of the file
readFilmFile :: IO Database
readFilmFile = do
    file <- readFile "films.txt"
    return (read file)

--This function writes the given data to the file
--Takes: Database (the database to be written to file)
writeFilmFile :: Database -> IO ()
writeFilmFile database = do
    writeFile "films.txt" (show database)

--This function asks the user for their name
--Returns: an IO User type representing the user's name
askUserName :: IO User
askUserName = do
    putStrLn "Please enter your name:"
    response <- getLine
    return response

--This is a prompt for the user to return to the main menu, when ready.
--Takes: Database (the database, to pass to the showMainMenu function), User (the username to pass along)
backToMainMenu :: Database -> User  -> IO ()
backToMainMenu database username = do
    putStrLn "\nEnter anything to return to the main menu..."
    input <- getLine
    showMainMenu database username

--This recursive function is used to display a navigation menu for the user to interact with the entire program.
--Takes: [File] (the current working database), User (the user name, used for some of the menu functions)
showMainMenu :: Database -> User  -> IO ()
showMainMenu database username = do
    putStrLn "\n"
    putStrLn ("Hello "++ username ++ "! Please enter an option number.")
    putStrLn "==========================================="
    putStrLn "1. Show all films"
    putStrLn "2. Show the good films (Average rating 7+)"
    putStrLn "3. Show films between two dates"
    putStrLn "4. Show films by a director"
    putStrLn "5. Show average website rating for a director"
    putStrLn "6. Add a film"
    putStrLn "7. Rate a film"
    putStrLn "8. Show a user's ratings"
    putStrLn "9. Exit\n"

    response <- getLine

    case response of 
        "1" -> do
            --Display all the films
            putStrLn (filmsAsString database)
            backToMainMenu database username
        "2" -> do
            --Display all the films with a rating of 7 or more
            putStrLn (getFilmsWithRatingSevenPlus database)
            backToMainMenu database username
        "3" -> menuOption3 database username
        "4" -> menuOption4 database username
        "5" -> menuOption5 database username
        "6" -> menuOption6 database username
        "7" -> menuOption7 database username
        "8" -> menuOption8 database username
        "9" -> do
            writeFilmFile database 
            return ()
        _ -> showMainMenu database username

--This function is used to handle the menu option 3, to show a list of films between two dates, sorted by website rating. 
--It is recursive, when input validation fails.
--Takes: Database (database to process), User (the user name)s
menuOption3 :: Database -> User -> IO ()
menuOption3 database username = do
    --Input and validate date 1
    putStrLn "Please enter the first date:"
    date1String <- getLine
    date1Valid <- validateIntAndRange date1String 1900 2016

    if (date1Valid) then do
        let date1 = read date1String

        --Input and validate date 2
        putStrLn "Please enter the second date:"
        date2String <- getLine
        date2Valid <- validateIntAndRange date2String 1900 2016

        if (date2Valid) then do
            let date2 = read date2String

            --Display the results from the helper functions used for processing the database with the inputs
            putStrLn (ratedFilmsAsString (getFilmsBetween database date1 date2))
            backToMainMenu database username
        else do
            menuOption3 database username
    else do
        menuOption3 database username

--This is used to handle menu option 4, to display all films directed by a director.
--This is also recursive when input validation fails. 
--Takes: Database (database to process), User (the user name)
menuOption4 :: Database -> User -> IO ()
menuOption4 database username = do 
    --Input and validate the director name
    putStrLn "Please enter the director name"
    director <- getLine
    directorValid <- validateString director

    if directorValid then do 
        --A no results message is displayed when there is no data returned from processing
        if(length (filmsAsString (getFilmsByDirector database director))==0) then do
            putStrLn "No results"
        else do
            putStrLn (filmsAsString (getFilmsByDirector database director))
        
        backToMainMenu database username
    else do 
        menuOption4 database username       

--This function handles menu option 5, to show the average rating from a given director. It is recursive when validation fails.
--Takes: Database (database to process), User (the user name)
menuOption5 :: Database -> User -> IO ()
menuOption5 database username = do
    --Input and validate director name
    putStrLn "Please enter the director name"
    director <- getLine
    directorValid <- validateString director

    if directorValid then do
        --Show a user friendly "No results" message when no data is returned
        if(getDirectorAverageRating database director>0) then do
            putStrLn (printf "%.4s" (show (getDirectorAverageRating database director)))
        else do
            putStrLn "No results"
        
        backToMainMenu database username
    else do
        menuOption5 database username
        
--This function handles menu option 6, to add a film to the database. All inputs are validated and on failure, the function becomes recursive.
--Takes: Database (database to process), User (the user name)
menuOption6 :: Database -> User -> IO ()
menuOption6 database username = do
    --Input and validate the title of the film
    putStrLn "Please enter the title of the film you would like to add"
    title <- getLine
    titleValid <- validateString title

    if titleValid then do
        --Check first that the film doesn't already exist in the database, to maintain data integrity
        if (filmExists database title) then do
            putStrLn "This film already exists"
            backToMainMenu database username
        else do
            --Input and validate the director name
            putStrLn "Please enter the director's name"
            director <- getLine
            directorValid <- validateString director

            if directorValid then do
                --Input and validate the year to be within the 1900-2016 range
                putStrLn "Please enter the year of release"
                yearString <- getLine
                let yearValid = isNum yearString

                if yearValid then do
                    --Give the user a confirmation message that the film has been added, and call the necessary functions
                    let year = read yearString
                    putStrLn "Film added"
                    backToMainMenu (addFilm title director year database) username
                else do
                    showNumError
                    menuOption6 database username
            else do
                menuOption6 database username
        else do
            menuOption6 database username

--This function handles menu option 7, to let the user rate a film. Failed input validation re-calls the function.
--Takes: Database (database to process), User (the user name)
menuOption7 :: Database -> User -> IO ()
menuOption7 database username = do
    --Input and validate the name of the film to rate
    putStrLn "Please enter the name of the film you would like to rate"
    title <- getLine
    titleValid <- validateString title

    if titleValid then do
        --First check that the film exists, to prevent any mishaps
        if (filmExists database title) then do
            --Input and validate the rating score to be a number between 0 and 10
            putStrLn "What rating would you like to enter for this film?"
            rating <- getLine
            ratingValid <- validateIntAndRange rating 0 10

            if ratingValid then do
                --Give the user a confirmation message, and proceed to apply the rating
                putStrLn "Rating added"
                backToMainMenu (rateFilm database title username (read rating)) username
            else do
                menuOption7 database username            
        else do
            putStrLn "This film is not in the database. Check your spelling and spaces, or add it from the main menu.\n"
            backToMainMenu database username
    else do
        menuOption7 database username

--This function handles the menu option 8, to display a certain user's ratings. Recursive when input validation fails
--Takes: Database (database to process), User (the user name)
menuOption8 :: Database -> User -> IO ()
menuOption8 database username = do
    --Input and validate user name
    putStrLn "Which user's ratings would you like to see?"
    user <- getLine
    userValid <- validateString user

    if userValid then do 
        --Check if there are any results, and display "No results" if there are none
        if length (listOfRatingsToString (database `hasARatingFrom` user)) == 0 then do
            putStrLn "No results"
        else do                
            putStrLn (listOfRatingsToString (database `hasARatingFrom` user))
        
        backToMainMenu database username
    else do
        menuOption8 database username