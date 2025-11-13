2+2
library(R6)


# Two Sum
# Given an array of integers nums and an integer target, return indices of the two numbers such that they add up to target.
# You may assume that each input would have exactly one solution, and you may not use the same element twice.
# You can return the answer in any order.

Solution <- R6Class("Solution",
  public = list(
    twoSum = function(nums, target) {
      n <- length(nums)
      for (i in 1:(n-1)){
        for (j in (i+1):n) {
          if(nums[i] + nums[j]==target)
          {
            return(c(i,j)) # R-like convention of 1-based index
          }
        }
      }
      print("No two sum solution")
      return(NULL)
    }
  )
)

s <- Solution$new()
s$twoSum(c(2,7,11,15), 9)


### LINKED LIST DEFINITON and helpers
ListNode <- R6Class("ListNode",
  public = list(
    val = 0,
    next_node = NULL,
    initialize = function(val = 0, next_node = NULL) {
      self$val <- val
      self$next_node <- next_node
    }
  )
)
print_list <- function(l) {
  vals <- c()
  while (!is.null(l)) {
    vals <- c(vals, l$val)
    l <- l$next_node
  }
  print(paste(vals))
}

n1 <- ListNode$new(1, ListNode$new(2, ListNode$new(3)))
n2 <- ListNode$new(2, ListNode$new(3))

n_a <- ListNode$new(9, ListNode$new(9, ListNode$new(9, ListNode$new(9, ListNode$new(9, ListNode$new(9, ListNode$new(9)))))))
n_b <- ListNode$new(9, ListNode$new(9, ListNode$new(9, ListNode$new(9))))
###

# You are given two non-empty linked lists representing two non-negative integers. 
# The digits are stored in reverse order, and each of their nodes contains a single digit. 
# Add the two numbers and return the sum as a linked list.
# You may assume the two numbers do not contain any leading zero, except the number 0 itself.

Solution <- R6Class("Solution",
  public = list(
    addTwoNumbers = function(l1, l2) {
        l_length <- function(l) {
            len <- 0
            while(!is.null(l)) {
                len <- len + 1
                l <- l$next_node
            }
            return(len)
        }

        n <- max(l_length(l1), l_length(l2))

        carry <- 0
        result <- ListNode$new(0)
        current <- result

        for (i in 1:n) {
            val_1 <- if (!is.null(l1)) l1$val else 0
            val_2 <- if (!is.null(l2)) l2$val else 0
            l1 <- if (!is.null(l1)) l1$next_node else NULL
            l2 <- if (!is.null(l2)) l2$next_node else NULL

            sum <- val_1 + val_2 + carry

            if (sum < 10) {
                digit <- sum
                carry <- 0
            } else {
                digit <- sum - 10
                carry <- 1
            }

            node <- ListNode$new(digit)
            current$next_node <- node
            current <- current$next_node
        }

        if (carry > 0) {
            node <- ListNode$new(carry)
            current$next_node <- node
            current <- current$next_node
        }

        return(result$next_node)
    }
  )
)


s <- Solution$new()

result <- s$addTwoNumbers(n1, n2)
result <- s$addTwoNumbers(n_a, n_b)

print_list(result)


# Longest Substring Without Repeating Characters
# Given a string s, find the length of the longest substring without duplicate characters.

Solution <- R6Class("Solution",
  public = list(
    lengthOfLongestSubstring = function(s) {
        n <- nchar(s)
        if (n == 0) {
            return(0)
        }

        substring <- substr(s, 1, 1)
        max_substring <- substr(s, 1, 1)

        i <- 2

        while (i <= n) {
            current_char <- substr(s, i, i)
            # print(current_char)

            if (!grepl(current_char, substring, fixed = TRUE)) {
                substring <- paste0(substring, current_char) #(add string to string)
                print("current_char added to substring:")
                print(substring)
            } else {
                found_index <- regexpr(current_char, substring, fixed = TRUE)[1]
                substring <- substr(substring, found_index + 1, nchar(substring))
                substring <- paste0(substring, current_char)
                print("new substring started:")
                print(substring)
            }
            i <- i + 1

            if (nchar(substring) > nchar(max_substring)) {
                max_substring <- substring
            }
        }
        print("Max substring:")
        print(max_substring)
        print("Length:")
        print(nchar(max_substring))
        return(nchar(max_substring))
    }
  )
)

s <- Solution$new()
result <- s$lengthOfLongestSubstring("abcazzzzzbcbbafdbeghaxzda")


# Median of Two Sorted Arrays

# Given two sorted arrays nums1 and nums2 of size m and n respectively, return the median of the two sorted arrays.

# TODO: 
# The overall run time complexity should be O(log (m+n)).

Solution <- R6Class("Solution",
  public = list(
    findMedianSortedArrays = function(nums1, nums2) {
        nums3 <- sort(c(nums1, nums2))
        n <- length(nums3)
        if (n %% 2 == 1) {
            median <- nums3[(n + 1) / 2]
        } else {
            median <- (nums3[n / 2] + nums3[n / 2 + 1]) / 2
        }
        return(median)
    }
  )
)

s <- Solution$new()
result <- s$findMedianSortedArrays(c(1, 2, 6, 7, 13), c(2, 4))
result <- s$findMedianSortedArrays(c(1, 2), c(3, 4))

result

# Longest Palindromic Substring

# Given a string s, return the longest palindromic substring in s.

Solution <- R6Class("Solution",
  public = list(
    longestPalindrome = function(s) {
        max_palindrome <- substring(s, 1, 1)
        for (i in 1:nchar(s)){
            substring <- substring(s, i, i)
            m <- i # start of the palindrome
            n <- i # end of the palindrome

            # starting palindrome
            while (n+1 <= nchar(s) - 1 && substring(s, n+1, n+1) == substring){
                substring <- paste0(substring, substring(s, n+1, n+1))
                n <- n + 1
            }

            # expanding palindrome
            while ((n+1 <= nchar(s)) && (m-1 >= 0) && (substring(s, m-1, m-1) == substring(s, n+1, n+1))){
                substring <- paste0(substring(s, m-1, m-1), substring, substring(s, n+1, n+1))
                m <- m - 1
                n <- n + 1
            }

            if (nchar(substring) > nchar(max_palindrome)){
                max_palindrome <- substring
            }
        }
        return(max_palindrome)
    }
  )
)

s <- Solution$new()
result <- s$longestPalindrome("adsedcbbdexd")
result

# Zigzag Conversion

# The string "PAYPALISHIRING" is written in a zigzag pattern on a given number of rows like this: (you may want to display this pattern in a fixed font for better legibility)
# P   A   H   N
# A P L S I I G
# Y   I   R
# And then read line by line: "PAHNAPLSIIGYIR"
# Write the code that will take a string and make this conversion given a number of rows:
# string convert(string s, int numRows);

Solution <- R6Class("Solution",
  public = list(
    convert = function(s, numRows) {
        step1 <- max(2 * numRows - 2, 1)
        print(paste("step1:", step1))
        result <- ""

        n <- nchar(s)

        for (i in (0:(numRows-1))) {
          print(paste("i:", i))

          # first and last rows
          if ((i == 0) || (i == numRows - 1)) {
            # substring <- s[seq(i + 1, nchar(s), by = step1)]
            indices <- seq(from = i + 1, to = n, by = step1)
            print("indices:")
            print(indices)

            chars <- substring(s, indices, indices)
            print("chars:")
            print(chars)

            substring_val <- paste(chars, collapse = "")
          } else {
            # middle rows
            idx_ver <- seq(from=i, to=n-1, by = step1)
            idx_dia <- seq(from=step1 - i, to=n-1, by = step1)
            all_indices <- c(idx_ver, idx_dia)

            sorted_indices <- sort(unique(all_indices)) + 1

            chars <- substring(s, sorted_indices, sorted_indices)
            substring_val <- paste(chars, collapse = "")
            print(paste("substring_val:", substring_val))
          }

          result <- paste0(result, substring_val)
          print(paste("result so far:", result))
        }
        return(result)
    }
  )
)

s <- Solution$new()
result <- s$convert("PAYPALISHIRING", 3)
result


# Reverse Integer

# Given a signed 32-bit integer x, return x with its digits reversed. If reversing x causes the value to go outside the signed 32-bit integer range [-231, 231 - 1], then return 0.
# Assume the environment does not allow you to store 64-bit integers (signed or unsigned).

Solution <- R6Class("Solution",
  public = list(
    reverse = function(x) {
      x_str <- as.character(x)

      if (substr(x_str, 1,1) == "-") {
        num_part <- substr(x_str, 2, nchar(x_str))
        chars <- strsplit(num_part, split = "")[[1]]
        reversed_part <- paste(rev(chars), collapse = "")

        x_rev <- as.numeric(paste0("-", reversed_part))
      } else {
        chars <- strsplit(x_str, split = "")[[1]]
        reversed_str <- paste(rev(chars), collapse = "")

        x_rev <- as.numeric(reversed_str)
      }
      if (x_rev < -2^31 || x_rev > 2^31 - 1) {
        return(0)
      } else {
        return(x_rev)
      }
    }
  )
)

s <- Solution$new()
result <- s$reverse(123)

result
