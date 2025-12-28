'use client';

import { AppConfig, UserSession, showConnect } from '@stacks/connect';
import { APP_DETAILS } from './stacks-config';

const appConfig = new AppConfig(['store_write', 'publish_data']);
export const userSession = new UserSession({ appConfig });

export interface WalletData {
  address: string;
  isConnected: boolean;
}

export const connectWallet = (): Promise<void> => {
  return new Promise((resolve, reject) => {
    showConnect({
      appDetails: APP_DETAILS,
      onFinish: () => {
        resolve();
      },
      onCancel: () => {
        reject(new Error('User cancelled wallet connection'));
      },
      userSession,
    });
  });
};

export const disconnectWallet = (): void => {
  userSession.signUserOut();
};

export const getWalletAddress = (): string | null => {
  if (typeof window === 'undefined') return null;

  if (userSession.isUserSignedIn()) {
    const userData = userSession.loadUserData();
    return userData.profile.stxAddress.mainnet;
  }
  return null;
};

export const isWalletConnected = (): boolean => {
  if (typeof window === 'undefined') return false;
  return userSession.isUserSignedIn();
};
