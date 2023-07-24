export const generateRandomNumber = () => Math.floor(Math.random() * 9999) + 1;

export const parseQueries = (n: string | null) => {
  if (n) {
    const queries = parseInt(n, 10) || 1;
    return queries >= 1 && queries <= 500 ? queries : 500;
  }

  return 1;
};
