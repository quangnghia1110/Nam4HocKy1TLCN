package studentConsulting.service.implement;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
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
    public List<ConsultantDTO> getAllConsultants() {
        List<UserInformationEntity> consultants = new ArrayList<>();
        userRepository.findAllByRoleName("TUVANVIEN").forEach(consultants::add);

        return consultants.stream()
            .map(userInfo -> ConsultantDTO.builder()
                .id(userInfo.getAccount().getId())
                .firstName(userInfo.getFirstName())
                .lastName(userInfo.getLastName())
                .email(userInfo.getAccount().getEmail())
                .phone(userInfo.getPhone())
                .avatarUrl(userInfo.getAvatarUrl())
                .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                .build())
            .collect(Collectors.toList());
    }

    
    public List<ConsultantDTO> getConsultantByDepartment(Integer departmentId){
    	 List<UserInformationEntity> consultants = new ArrayList<>();
         userRepository.findAllByRoleNameAndDepartment("TUVANVIEN", departmentId).forEach(consultants::add);
        return consultants.stream()
                .map(userInfo -> ConsultantDTO.builder()
                    .id(userInfo.getAccount().getId())
                    .firstName(userInfo.getFirstName())
                    .lastName(userInfo.getLastName())
                    .email(userInfo.getAccount().getEmail())
                    .phone(userInfo.getPhone())
                    .avatarUrl(userInfo.getAvatarUrl())
                    .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                    .build())
                .collect(Collectors.toList());
    }
    
    @Override
    public List<ConsultantDTO> searchConsultantsByName(String name) {
        List<UserInformationEntity> consultants = userRepository.findByName(name);
        return consultants.stream()
                .map(userInfo -> ConsultantDTO.builder()
                    .id(userInfo.getAccount().getId())
                    .firstName(userInfo.getFirstName())
                    .lastName(userInfo.getLastName())
                    .email(userInfo.getAccount().getEmail())
                    .phone(userInfo.getPhone())
                    .avatarUrl(userInfo.getAvatarUrl())
                    .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                    .build())
                .collect(Collectors.toList());
    }

}


