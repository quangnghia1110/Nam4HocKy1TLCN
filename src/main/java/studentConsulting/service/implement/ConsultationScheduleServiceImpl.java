package studentConsulting.service.implement;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultationScheduleService;

@Service
public class ConsultationScheduleServiceImpl implements IConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Override
    public ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request, UserInformationEntity user) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        // Kiểm tra sự tồn tại của tư vấn viên
        Optional<UserInformationEntity> consultantOpt = userRepository.findById(request.getConsultantId());
        if (!consultantOpt.isPresent()) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không tồn tại"));
        }

        // Kiểm tra sự tồn tại của phòng ban
        Optional<DepartmentEntity> departmentOpt = departmentRepository.findById(request.getDepartmentId());
        if (!departmentOpt.isPresent()) {
            errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
        }

        // Kiểm tra nếu có lỗi thì ném ngoại lệ
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        UserInformationEntity consultant = consultantOpt.get();
        DepartmentEntity department = departmentOpt.get();

        // Kiểm tra tư vấn viên có thuộc phòng ban hay không
        if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc phòng ban đã chọn"));
            throw new CustomFieldErrorException(errors);
        }

        // Tạo lịch tư vấn
        ConsultationScheduleEntity schedule = new ConsultationScheduleEntity();
        schedule.setUser(user);
        schedule.setConsultant(consultant);
        schedule.setDepartment(department);
        schedule.setTitle(request.getTitle());
        schedule.setContent(request.getContent());
        schedule.setStatusPublic(request.getStatusPublic());

        // Lưu lịch tư vấn
        ConsultationScheduleEntity savedSchedule = consultationScheduleRepository.save(schedule);

        return mapToDTO(savedSchedule);
    }

    


    // Hàm chuyển đổi từ entity sang DTO
    private ConsultationScheduleDTO mapToDTO(ConsultationScheduleEntity schedule) {
        return ConsultationScheduleDTO.builder()
                .departmentId(schedule.getDepartment().getId())
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .statusPublic(schedule.getStatusPublic())
                .consultantName(schedule.getConsultant().getLastName() + " " + schedule.getConsultant().getFirstName())
                .userName(schedule.getUser().getLastName() + " " + schedule.getUser().getFirstName())
                .build();
    }
    
    @Override
    public Page<ConsultationScheduleDTO> getConsultationsByUserAndDepartmentAndTitle(UserInformationEntity user, Integer departmentId, String title, Pageable pageable) {
        return consultationScheduleRepository
                .findByUserAndDepartmentIdAndTitleContaining(user, departmentId, title, pageable)
                .map(this::mapToDTO);
    }

    @Override
    public Page<ConsultationScheduleDTO> getConsultationsByUserAndDepartment(UserInformationEntity user, Integer departmentId, Pageable pageable) {
        return consultationScheduleRepository
                .findByUserAndDepartmentId(user, departmentId, pageable)
                .map(this::mapToDTO);
    }

    @Override
    public Page<ConsultationScheduleDTO> searchConsultationsByUserAndTitle(UserInformationEntity user, String title, Pageable pageable) {
        return consultationScheduleRepository
                .findByUserAndTitleContaining(user, title, pageable)
                .map(this::mapToDTO);
    }

    @Override
    public Page<ConsultationScheduleDTO> getAllConsultationsByUser(UserInformationEntity user, Pageable pageable) {
        return consultationScheduleRepository
                .findByUser(user, pageable)
                .map(this::mapToDTO);
    }
    
}
