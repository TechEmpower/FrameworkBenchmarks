//
//  ByteBuffer+FEData.swift
//  
//
//  Created by Yakov Shapovalov on 04.07.2024.
//

import FoundationPreview
import NIOCore

typealias FEData = FoundationEssentials.Data

extension ByteBuffer {

    func getData(at index: Int, length: Int) -> FEData? {
        return self.getData(at: index, length: length, byteTransferStrategy: .automatic)
    }

    func getData(at index0: Int, length: Int, byteTransferStrategy: ByteTransferStrategy) -> FEData? {
        let index = index0 - self.readerIndex
        guard index >= 0 && length >= 0 && index <= self.readableBytes - length else {
            return nil
        }
        let doCopy: Bool
        switch byteTransferStrategy {
        case .copy:
            doCopy = true
        case .noCopy:
            doCopy = false
        case .automatic:
            doCopy = length <= 256*1024
        }

        return self.withUnsafeReadableBytesWithStorageManagement { ptr, storageRef in
            if doCopy {
                return FEData(bytes: UnsafeMutableRawPointer(mutating: ptr.baseAddress!.advanced(by: index)),
                            count: Int(length))
            } else {
                _ = storageRef.retain()
                return FEData(bytesNoCopy: UnsafeMutableRawPointer(mutating: ptr.baseAddress!.advanced(by: index)),
                            count: Int(length),
                            deallocator: .custom { _, _ in storageRef.release() })
            }
        }
    }
}
