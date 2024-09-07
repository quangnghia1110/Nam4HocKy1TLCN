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

    // Hàm chuyển đổi từ UserInformationEntity sang ConsultantDTO
    private ConsultantDTO mapToConsultantDTO(UserInformationEntity userInfo) {
        return ConsultantDTO.builder()
                .id(userInfo.getAccount().getId())
                .firstName(userInfo.getFirstName())
                .lastName(userInfo.getLastName())
                .email(userInfo.getAccount().getEmail())
                .phone(userInfo.getPhone())
                .avatarUrl(userInfo.getAvatarUrl())
                .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                .build();
    }

    // Lấy tất cả tư vấn viên với phân trang
    @Override
    public Page<ConsultantDTO> getAllConsultants(Pageable pageable) {
        Page<UserInformationEntity> consultantsPage = userRepository.findAllByRoleName("TUVANVIEN", pageable);
        return consultantsPage.map(this::mapToConsultantDTO); // Sử dụng hàm mapToConsultantDTO
    }

    // Lấy tư vấn viên theo phòng ban với phân trang
    @Override
    public Page<ConsultantDTO> getConsultantByDepartment(Integer departmentId, Pageable pageable) {
        Page<UserInformationEntity> consultantsPage = userRepository.findAllByRoleNameAndDepartment("TUVANVIEN", departmentId, pageable);
        return consultantsPage.map(this::mapToConsultantDTO); // Sử dụng hàm mapToConsultantDTO
    }

    // Tìm kiếm tư vấn viên theo tên với phân trang
    @Override
    public Page<ConsultantDTO> searchConsultantsByName(String firstName, Pageable pageable) {
        Page<UserInformationEntity> consultants = userRepository.findByFirstNameAndRoleName(firstName, "TUVANVIEN", pageable);
        return consultants.map(this::mapToConsultantDTO); // Sử dụng hàm mapToConsultantDTO
    }

    // Tìm kiếm tư vấn viên theo phòng ban và tên
    @Override
    public Page<ConsultantDTO> getConsultantsByDepartmentAndName(Integer departmentId, String firstName, Pageable pageable) {
        Page<UserInformationEntity> consultantsPage = userRepository.findByDepartmentAndFirstNameAndRoleName(departmentId, firstName, "TUVANVIEN", pageable);
        return consultantsPage.map(this::mapToConsultantDTO); // Sử dụng hàm mapToConsultantDTO
    }
}



