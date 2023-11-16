# Trip Planner

This code implements a Trip Planner in Racket using the DSSL2 (Data Structures and Software Libraries 2) language. The Trip Planner allows users to find points of interest, plan routes, and discover nearby locations based on specified categories. The implementation includes data structures such as a weighted undirected graph (WUGraph), association lists, hash tables, and a binary heap for efficient operations.

## Features:

- **Locate All**: Find the positions of all points of interest within a given category.
- **Plan Route**: Plan the shortest route from a source position to a specified point of interest.
- **Find Nearby**: Discover a specified number of points of interest of a given category that are closest to a source position.

## Implementation Details:

- **Data Types**: Utilizes various data types such as latitude and longitude vectors, raw positions, raw road segments, and raw points of interest.
- **Contracts**: Contracts are defined for the functions provided by the Trip Planner interface, ensuring proper input and output types.
- **Graph Representation**: The weighted undirected graph (WUGraph) is used to represent road segments and their connections.
- **Associative Data Structures**: Association lists and hash tables are employed to efficiently store and retrieve data, providing fast lookups.
- **Dijkstra's Algorithm**: The Dijkstra algorithm is implemented for finding the shortest paths in the graph.
- **Error Handling**: Includes error checks for scenarios such as missing points of interest or unreachable destinations.

## Usage:

Example usage and tests are provided at the end of the code, demonstrating the functionality of the Trip Planner in scenarios like locating points of interest, planning routes, and finding nearby places.

## Principles:

The code follows ethical principles, including acknowledgment of sources, protection of work, and honesty in discussions with instructors.
