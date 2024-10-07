package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.address.ManageAddressDTO;
import studentConsulting.model.payload.request.address.AddressRequest;

import java.util.Optional;

public interface IAdminAdressService {

    ManageAddressDTO createAddress(AddressRequest addressRequest);

    ManageAddressDTO updateAddress(Integer id, AddressRequest addressRequest);

    void deleteAddressById(Integer id);

    ManageAddressDTO getAddressById(Integer id);

    Page<ManageAddressDTO> getAllAddresses(Pageable pageable);

    Page<ManageAddressDTO> getAllAddressesWithFilters(Optional<String> line, Optional<String> provinceCode, Optional<String> districtCode, Optional<String> wardCode, Pageable pageable);
}
