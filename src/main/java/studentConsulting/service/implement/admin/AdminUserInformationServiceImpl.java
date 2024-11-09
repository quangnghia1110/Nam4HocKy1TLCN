package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
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
        Specification<UserInformationEntity> spec = Specification.where((root, query, cb) -> cb.equal(root.get("account").get("id"), accountId));

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
}
