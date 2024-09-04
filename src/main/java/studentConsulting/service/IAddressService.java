package studentConsulting.service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.AddressDTO;

public interface IAddressService {
    public void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO);}
