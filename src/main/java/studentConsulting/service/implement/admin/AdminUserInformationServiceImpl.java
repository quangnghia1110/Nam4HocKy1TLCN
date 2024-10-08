package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.entity.address.WardEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.user.ManageUserDTO;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.repository.address.WardRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.admin.IAdminUserInformationService;
import studentConsulting.specification.user.UserInformationSpecification;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminUserInformationServiceImpl implements IAdminUserInformationService {

    @Autowired
    private UserRepository userInformationRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private WardRepository wardRepository;
    
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

    @Override
    public void importUsers(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageUserDTO> users = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String firstName = row.get(1);
                        String lastName = row.get(2);
                        String studentCode = row.get(3);
                        String gender = row.get(4);
                        String phone = row.get(5);
                        String schoolName = row.get(6);
                        LocalDate createdAt = LocalDate.parse(row.get(7));
                        String line = row.get(8);
                        String province = row.get(9);
                        String district = row.get(10);
                        String ward = row.get(11);

                        ManageUserDTO.AddressDTO addressDTO = ManageUserDTO.AddressDTO.builder()
                                .line(line)
                                .provinceFullName(province)
                                .districtFullName(district)
                                .wardFullName(ward)
                                .build();

                        return ManageUserDTO.builder()
                                .id(id)
                                .firstName(firstName)
                                .lastName(lastName)
                                .studentCode(studentCode)
                                .gender(gender)
                                .phone(phone)
                                .schoolName(schoolName)
                                .createdAt(createdAt)
                                .address(addressDTO)
                                .build();
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu người dùng: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        users.forEach(user -> {
            try {
                UserInformationEntity entity = new UserInformationEntity();
                entity.setId(user.getId());
                entity.setFirstName(user.getFirstName());
                entity.setLastName(user.getLastName());
                entity.setStudentCode(user.getStudentCode());
                entity.setGender(user.getGender());
                entity.setPhone(user.getPhone());
                entity.setSchoolName(user.getSchoolName());
                entity.setCreatedAt(user.getCreatedAt());

                AddressEntity addressEntity = new AddressEntity();
                addressEntity.setLine(user.getAddress().getLine());

                ProvinceEntity provinceEntity = provinceRepository.findByFullName(user.getAddress().getProvinceFullName())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh với tên: " + user.getAddress().getProvinceFullName()));

                DistrictEntity districtEntity = districtRepository.findByFullName(user.getAddress().getDistrictFullName())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy quận với tên: " + user.getAddress().getDistrictFullName()));

                WardEntity wardEntity = wardRepository.findByFullName(user.getAddress().getWardFullName())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phường với tên: " + user.getAddress().getWardFullName()));

                addressEntity.setProvince(provinceEntity);
                addressEntity.setDistrict(districtEntity);
                addressEntity.setWard(wardEntity);

                entity.setAddress(addressEntity);

                userInformationRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu người dùng vào cơ sở dữ liệu: " + e.getMessage());
            }
        });
    }

}
