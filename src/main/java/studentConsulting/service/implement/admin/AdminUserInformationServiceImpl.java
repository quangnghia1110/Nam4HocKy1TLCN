package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
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
    public ManageUserDTO updateUserInformation(Integer id, ManageUserDTO userRequest) {
        UserInformationEntity userEntity = userInformationRepository.findById(id)
                .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người dùng với ID: " + id));

        userEntity.setFirstName(userRequest.getFirstName());
        userEntity.setLastName(userRequest.getLastName());
        userEntity.setPhone(userRequest.getPhone());
        userEntity.setGender(userRequest.getGender());
        userEntity.setSchoolName(userRequest.getSchoolName());
        userEntity.setStudentCode(userRequest.getStudentCode());

        if (userRequest.getAddress() != null) {
            AddressEntity address = userEntity.getAddress() != null ? userEntity.getAddress() : new AddressEntity();

            address.setLine(userRequest.getAddress().getLine());
            address.setProvince(provinceRepository.findByFullName(userRequest.getAddress().getProvinceFullName())
                    .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh/thành phố")));
            address.setDistrict(districtRepository.findByFullName(userRequest.getAddress().getDistrictFullName())
                    .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy quận/huyện")));
            address.setWard(wardRepository.findByFullName(userRequest.getAddress().getWardFullName())
                    .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phường/xã")));

            userEntity.setAddress(address);
        }

        UserInformationEntity updatedUser = userInformationRepository.save(userEntity);

        return userInformationMapper.mapToDTO(updatedUser);
    }

}
