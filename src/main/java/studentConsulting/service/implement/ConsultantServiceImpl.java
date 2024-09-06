package studentConsulting.service.implement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultantService;

@Service
public class ConsultantServiceImpl implements IConsultantService {

    @Autowired
    private UserRepository userRepository;

    //StreamSupport giúp hỗ trợ chuyển Iterable sang Stream
    @Override
    public Page<ConsultantDTO> getAllConsultants(Pageable pageable) {
        Page<UserInformationEntity> consultantsPage = userRepository.findAllByRoleName("TUVANVIEN", pageable);
        return consultantsPage.map(userInfo -> ConsultantDTO.builder()
                .id(userInfo.getAccount().getId())
                .firstName(userInfo.getFirstName())
                .lastName(userInfo.getLastName())
                .email(userInfo.getAccount().getEmail())
                .phone(userInfo.getPhone())
                .avatarUrl(userInfo.getAvatarUrl())
                .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                .build());
    }
    
    @Override
    public Page<ConsultantDTO> getConsultantByDepartment(Integer departmentId, Pageable pageable) {
        Page<UserInformationEntity> consultantsPage = userRepository.findAllByRoleNameAndDepartment("TUVANVIEN", departmentId, pageable);
        return consultantsPage.map(userInfo -> ConsultantDTO.builder()
                .id(userInfo.getAccount().getId())
                .firstName(userInfo.getFirstName())
                .lastName(userInfo.getLastName())
                .email(userInfo.getAccount().getEmail())
                .phone(userInfo.getPhone())
                .avatarUrl(userInfo.getAvatarUrl())
                .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                .build());
    }

    
    public Page<ConsultantDTO> searchConsultantsByName(String firstName, Pageable pageable) {
        Page<UserInformationEntity> consultants = userRepository.findByFirstNameAndRoleName(firstName, "TUVANVIEN", pageable);
        
        return consultants.map(userInfo -> ConsultantDTO.builder()
            .id(userInfo.getAccount().getId())
            .firstName(userInfo.getFirstName())
            .lastName(userInfo.getLastName())
            .email(userInfo.getAccount().getEmail())
            .phone(userInfo.getPhone())
            .avatarUrl(userInfo.getAvatarUrl())
            .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
            .build());
    }


}


