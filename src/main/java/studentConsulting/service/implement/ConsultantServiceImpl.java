package studentConsulting.service.implement;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.RoleEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.ManageUserInformationDTO;
import studentConsulting.model.payload.dto.UserDTO;
import studentConsulting.repository.RoleRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultantService;
import studentConsulting.specification.ConsultantSpecification;
import studentConsulting.specification.ManageSpecification;

@Service
public class ConsultantServiceImpl implements IConsultantService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;
    
    @Override
    public Page<ConsultantDTO> getFilteredConsultants(Integer departmentId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<UserInformationEntity> spec = Specification.where(ConsultantSpecification.hasRole("ROLE_TUVANVIEN"));

        if (departmentId != null) {
            spec = spec.and(ConsultantSpecification.hasDepartment(departmentId));
        }

        if (name != null && !name.trim().isEmpty()) {
            spec = spec.and(ConsultantSpecification.hasName(name.trim()));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultantSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultantSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultantSpecification.hasDateBefore(endDate));
        }

        return userRepository.findAll(spec, pageable).map(this::mapToConsultantDTO);
    }

    @Override
    public List<UserDTO> getConsultantsByDepartment(Integer departmentId) {
        List<UserInformationEntity> consultants = userRepository.findAll().stream()
            .filter(user -> user.getAccount().getRole().getName().equals("ROLE_TUVANVIEN") &&
                            user.getAccount().getDepartment().getId().equals(departmentId))
            .collect(Collectors.toList());
        
        return consultants.stream()
                .map(consultant -> new UserDTO(consultant.getId(), consultant.getFirstName(), consultant.getLastName()))
                .collect(Collectors.toList());
    }
    
    private ConsultantDTO mapToConsultantDTO(UserInformationEntity userInfo) {
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    @Override
    public Page<ManageUserInformationDTO> getConsultantsByManagerWithFilters(LocalDate startDate, LocalDate endDate, Pageable pageable, Principal principal) {
        String managerEmail = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(managerEmail);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        Specification<UserInformationEntity> spec = Specification
            .where(ManageSpecification.hasDepartment(departmentId))
            .and(ManageSpecification.hasRole("ROLE_TUVANVIEN"));

        if (startDate != null && endDate != null) {
            spec = spec.and(ManageSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ManageSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ManageSpecification.hasDateBefore(endDate));
        }

        Page<UserInformationEntity> consultantEntities = userRepository.findAll(spec, pageable);

        return consultantEntities.map(this::mapToDTO);
    }

    @Transactional
    @Override
    public void updateConsultantRoleToUser(Integer id, Principal principal) {
        String managerEmail = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(managerEmail);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer managerDepartmentId = manager.getAccount().getDepartment().getId();

        UserInformationEntity consultant = userRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tư vấn viên với ID này"));

        if (!consultant.getAccount().getDepartment().getId().equals(managerDepartmentId)) {
            throw new ErrorException("Không có quyền thay đổi role của tư vấn viên này vì tư vấn viên không thuộc phòng ban của bạn");
        }

        RoleEntity userRole = roleRepository.findByRoleName("ROLE_USER")
                .orElseThrow(() -> new ErrorException("Không tìm thấy vai trò ROLE_USER"));
        
        consultant.getAccount().setRole(userRole);
        
        userRepository.save(consultant);
    }

    private ManageUserInformationDTO mapToDTO(UserInformationEntity entity) {
        return ManageUserInformationDTO.builder()
                .user(ManageUserInformationDTO.UserDTO.builder()
                		.id(entity.getId())
                        .studentCode(entity.getStudentCode())
                        .firstName(entity.getFirstName())
                        .lastName(entity.getLastName())
                        .phone(entity.getPhone())
                        .avatarUrl(entity.getAvatarUrl())
                        .gender(entity.getGender())
                        .schoolName(entity.getSchoolName())
                        .build())
                .account(ManageUserInformationDTO.AccountDTO.builder()
                        .email(entity.getAccount().getEmail())
                        .username(entity.getAccount().getUsername())
                        .build())
                .address(ManageUserInformationDTO.AddressDTO.builder()
                        .line(entity.getAddress().getLine())
                        .province(ManageUserInformationDTO.ProvinceDTO.builder()
                                .code(entity.getAddress().getProvince().getCode())
                                .fullName(entity.getAddress().getProvince().getFullName())
                                .build())
                        .district(ManageUserInformationDTO.DistrictDTO.builder()
                                .code(entity.getAddress().getDistrict().getCode())
                                .fullName(entity.getAddress().getDistrict().getFullName())
                                .build())
                        .ward(ManageUserInformationDTO.WardDTO.builder()
                                .id(entity.getAddress().getWard().getCode())
                                .fullName(entity.getAddress().getWard().getFullName())
                                .build())
                        .build())
                .department(ManageUserInformationDTO.DepartmentDTO.builder()
                        .id(entity.getAccount().getDepartment().getId())
                        .name(entity.getAccount().getDepartment().getName())
                        .build())
                .createdAt(entity.getCreatedAt())
                .build();
    }
}



