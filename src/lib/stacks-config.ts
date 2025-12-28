import { StacksMainnet, StacksTestnet } from '@stacks/network';

export const NETWORK = process.env.NEXT_PUBLIC_NETWORK === 'mainnet'
  ? new StacksMainnet()
  : new StacksTestnet();

export const CONTRACT_ADDRESS = process.env.NEXT_PUBLIC_CONTRACT_ADDRESS || 'SP3BXJENEWVNCFYGJF75DFS478H1BZJXNZPT84EAD';

export const CONTRACTS = {
  ASSET_LIBRARY: 'asset-library',
  STATS_TRACKER: 'stats-tracker',
  OFFER_EXCHANGE: 'offer-exchange',
  PROMO_MANAGER: 'promo-manager',
  MEDIATION_HUB: 'mediation-hub',
  FUNDS_KEEPER: 'funds-keeper',
  VOTE_HANDLER: 'vote-handler',
  ORACLE_LINK: 'oracle-link',
  CASH_DISTRIBUTOR: 'cash-distributor',
  PARTNER_HUB: 'partner-hub',
  PRIZE_POOL: 'prize-pool',
  SUBSCRIPTION_HUB: 'subscription-hub',
  AUDIENCE_SELECTOR: 'audience-selector',
  USER_PROFILES: 'user-profiles',
  THREAT_DETECTOR: 'threat-detector',
} as const;

export const APP_DETAILS = {
  name: 'AdStack',
  icon: typeof window !== 'undefined' ? `${window.location.origin}/logo.png` : '/logo.png',
};

export const MICRO_STX = 1000000;

export function microStxToStx(microStx: number | bigint): number {
  return Number(microStx) / MICRO_STX;
}

export function stxToMicroStx(stx: number): bigint {
  return BigInt(Math.floor(stx * MICRO_STX));
}
