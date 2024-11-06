package studentConsulting.model.payload.mapper.admin;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.ConsultantDTO;
import studentConsulting.model.payload.dto.actor.DepartmentDTO;
import studentConsulting.model.payload.dto.manage.ManageUserDTO;

@Component
public class UserInformationMapper {
    public ManageUserDTO mapToDTO(UserInformationEntity userInformationEntity) {
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

                .address(userInformationEntity.getAddress() != null ?
                        ManageUserDTO.AddressDTO.builder()
                                .line(userInformationEntity.getAddress().getLine())
                                .provinceFullName(userInformationEntity.getAddress().getProvince() != null ?
                                        userInformationEntity.getAddress().getProvince().getFullName() : null)
                                .districtFullName(userInformationEntity.getAddress().getDistrict() != null ?
                                        userInformationEntity.getAddress().getDistrict().getFullName() : null)
                                .wardFullName(userInformationEntity.getAddress().getWard() != null ?
                                        userInformationEntity.getAddress().getWard().getFullName() : null)
                                .build()
                        : null)

                .build();
    }

    public ConsultantDTO mapDTO(UserInformationEntity userInfo) {
        return ConsultantDTO.builder()
                .id(userInfo.getAccount().getId())
                .firstName(userInfo.getFirstName())
                .lastName(userInfo.getLastName())
                .email(userInfo.getAccount().getEmail())
                .phone(userInfo.getPhone())
                .avatarUrl(userInfo.getAvatarUrl())
                .department(userInfo.getAccount().getDepartment() != null
                        ? new DepartmentDTO(
                        userInfo.getAccount().getDepartment().getId(),
                        userInfo.getAccount().getDepartment().getName()
                )
                        : null)
                .build();
    }

}
