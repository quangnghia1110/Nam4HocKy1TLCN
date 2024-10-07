package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.user.ManageUserDTO;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.admin.IAdminUserInformationService;
import studentConsulting.specification.user.UserInformationSpecification;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdminUserInformationServiceImpl implements IAdminUserInformationService {

    @Autowired
    private UserRepository userInformationRepository;

    @Override
    public Page<ManageUserDTO> getAllUsersWithFilters(Optional<String> name, Optional<String> studentCode, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable) {
        Specification<UserInformationEntity> spec = Specification.where(null);

        if (name.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasName(name.get()));
        }

        if (studentCode.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasStudentCode(studentCode.get()));
        }

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasExactDateRange(startDate.get(), endDate.get()));
        } else if (startDate.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasExactStartDate(startDate.get()));
        } else if (endDate.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasDateBefore(endDate.get()));
        }

        Page<UserInformationEntity> userEntities = userInformationRepository.findAll(spec, pageable);
        return userEntities.map(this::mapToDTO);
    }


    @Override
    public ManageUserDTO getUserById(Integer id) {
        UserInformationEntity userInformation = userInformationRepository.findById(id)
                .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người dùng với ID: " + id));
        return mapToDTO(userInformation);
    }

    private ManageUserDTO mapToDTO(UserInformationEntity userInformationEntity) {
        return ManageUserDTO.builder()
                .id(userInformationEntity.getId())
                .avatarUrl(userInformationEntity.getAvatarUrl())
                .createdAt(userInformationEntity.getCreatedAt())
                .firstName(userInformationEntity.getFirstName())
                .lastName(userInformationEntity.getLastName())
                .gender(userInformationEntity.getGender())
                .phone(userInformationEntity.getPhone())
                .schoolName(userInformationEntity.getSchoolName())
                .studentCode(userInformationEntity.getStudentCode())
                .address(userInformationEntity.getAddress() != null ? mapToAddressDTO(userInformationEntity.getAddress()) : null)
                .build();
    }

    private ManageUserDTO.AddressDTO mapToAddressDTO(AddressEntity addressEntity) {
        return ManageUserDTO.AddressDTO.builder()
                .line(addressEntity.getLine())
                .provinceFullName(addressEntity.getProvince() != null ? addressEntity.getProvince().getFullName() : null)
                .districtFullName(addressEntity.getDistrict() != null ? addressEntity.getDistrict().getFullName() : null)  // Đổi tên thành districtFullName
                .wardFullName(addressEntity.getWard() != null ? addressEntity.getWard().getFullName() : null)  // Đổi tên thành wardFullName
                .build();
    }
}
