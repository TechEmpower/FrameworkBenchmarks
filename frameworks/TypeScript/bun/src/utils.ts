export const generateRandomNumber = () => Math.floor(Math.random() * 9999) + 1;

const regexGetN = /\?n=([^&]*)/;

export const parseQueries = (n: string | null) => {
  if (n) {
    const match = regexGetN.exec(n);
    if (match) {
      const queries = parseInt(match[1], 10) || 1;
      return queries >= 1 && queries <= 500 ? queries : 500;
    }
  }

  return 1;
};
