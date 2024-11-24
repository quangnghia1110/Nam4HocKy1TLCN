package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.AddressEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.actor.UserInformationDTO;
import studentConsulting.model.payload.dto.manage.ManageUserDTO;
import studentConsulting.model.payload.mapper.admin.UserInformationMapper;
import studentConsulting.repository.admin.DistrictRepository;
import studentConsulting.repository.admin.ProvinceRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.repository.admin.WardRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.admin.IAdminUserInformationService;
import studentConsulting.specification.actor.UserInformationSpecification;

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

    @Autowired
    private UserInformationMapper userInformationMapper;

    @Autowired
    private FileStorageServiceImpl fileStorageService;
    @Override
    public Page<ManageUserDTO> getUserByAdmin(Integer accountId, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable) {
        Specification<UserInformationEntity> spec = Specification.where(null);

        if (accountId != null) {
            spec = spec.and((root, query, cb) -> cb.equal(root.get("account").get("id"), accountId));
        }

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasExactDateRange(startDate.get(), endDate.get()));
        } else if (startDate.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasExactStartDate(startDate.get()));
        } else if (endDate.isPresent()) {
            spec = spec.and(UserInformationSpecification.hasDateBefore(endDate.get()));
        }

        Page<UserInformationEntity> userEntities = userInformationRepository.findAll(spec, pageable);

        return userEntities.map(userInformationMapper::mapToDTO);
    }



    @Override
    public ManageUserDTO getUserById(Integer id) {
        UserInformationEntity userInformation = userInformationRepository.findById(id)
                .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người dùng với ID: " + id));
        return userInformationMapper.mapToDTO(userInformation);
    }

    @Override
    public ManageUserDTO updateUserInformation(
            Integer id, String firstName, String lastName, String phone, String gender,
            String schoolName, String studentCode, String addressLine, String provinceFullName,
            String districtFullName, String wardFullName, MultipartFile avatarUrl) {

        UserInformationEntity userEntity = userInformationRepository.findById(id)
                .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người dùng với ID: " + id));

        if (firstName != null) userEntity.setFirstName(firstName);
        if (lastName != null) userEntity.setLastName(lastName);
        if (phone != null) userEntity.setPhone(phone);
        if (gender != null) userEntity.setGender(gender);
        if (schoolName != null) userEntity.setSchoolName(schoolName);
        if (studentCode != null) userEntity.setStudentCode(studentCode);

        if (addressLine != null || provinceFullName != null || districtFullName != null || wardFullName != null) {
            AddressEntity address = userEntity.getAddress() != null ? userEntity.getAddress() : new AddressEntity();

            if (addressLine != null) address.setLine(addressLine);
            if (provinceFullName != null) {
                address.setProvince(provinceRepository.findByFullName(provinceFullName)
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh/thành phố: " + provinceFullName)));
            }
            if (districtFullName != null) {
                address.setDistrict(districtRepository.findByFullName(districtFullName)
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy quận/huyện: " + districtFullName)));
            }
            if (wardFullName != null) {
                address.setWard(wardRepository.findByFullName(wardFullName)
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phường/xã: " + wardFullName)));
            }

            userEntity.setAddress(address);
        }

        if (avatarUrl != null && !avatarUrl.isEmpty()) {
            if (userEntity.getAvatarUrl() != null) {
                fileStorageService.deleteFile(userEntity.getAvatarUrl());
            }
            String fileName = fileStorageService.saveFile(avatarUrl);
            userEntity.setAvatarUrl(fileName);
        } else {
            if (userEntity.getAvatarUrl() != null) {
                fileStorageService.deleteFile(userEntity.getAvatarUrl());
                userEntity.setAvatarUrl(null);
            }
        }

        UserInformationEntity updatedUser = userInformationRepository.save(userEntity);

        return userInformationMapper.mapToDTO(updatedUser);
    }



}
