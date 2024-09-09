package studentConsulting.service.implement;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultationScheduleService;
import studentConsulting.specification.ConsultationScheduleSpecification;

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
        schedule.setMode(request.getMode());
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
                .consultationDate(schedule.getConsultationDate())  
                .consultationTime(schedule.getConsultationTime())  
                .location(schedule.getLocation())  
                .link(schedule.getLink()) 
                .mode(schedule.getMode()) 
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed()) 
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
    
    @Override
    public Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, Pageable pageable) {

        // Bắt đầu với một truy vấn cơ bản tìm theo tư vấn viên (consultant)
        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasConsultant(consultant));
        if (title != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasTitle(title));
        }

        if (statusPublic != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusPublic(statusPublic));
        }

        if (statusConfirmed != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusConfirmed(statusConfirmed));
        }

        if (mode != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasMode(mode));
        }

        return consultationScheduleRepository.findAll(spec, pageable)
                .map(this::mapToDTO);
    }

    
    
    @Override
    public void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request, UserInformationEntity consultant) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        // Lấy lịch tư vấn
        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new RuntimeException("Lịch tư vấn không tồn tại"));

        // Kiểm tra nếu lịch đã xác nhận
        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
        	errors.add(new FieldErrorDetail("schedule","Lịch đã được xác nhận trước đó."));
        }

        // Kiểm tra loại tư vấn (online/offline)
        if (request.getStatusConfirmed()) {
            if (schedule.getMode() != null && schedule.getMode()) { // Online
            	if (request.getLocation() != null) {
            		errors.add(new FieldErrorDetail("schedule","Không được phép nhập địa điểm cho tư vấn online."));
                }
            	if (request.getLink() == null || request.getConsulationDate() == null || request.getConsultationTime() == null) {
                	errors.add(new FieldErrorDetail("schedule","Phải cung cấp đầy đủ thông tin link, ngày và giờ cho tư vấn online."));
                }
                schedule.setLink(request.getLink());
            } else { // Offline
            	if (request.getLink() != null) {
            		errors.add(new FieldErrorDetail("schedule","Không được phép nhập link cho tư vấn offline."));
                }
            	if (request.getLocation() == null || request.getConsulationDate() == null || request.getConsultationTime() == null) {
                	errors.add(new FieldErrorDetail("schedule","Phải cung cấp đầy đủ thông tin địa điểm, ngày và giờ cho tư vấn offline."));
                }
                schedule.setLocation(request.getLocation());
            }

            // Cập nhật trạng thái xác nhận
            schedule.setStatusConfirmed(true);
            schedule.setConsultationDate(request.getConsulationDate());
            schedule.setConsultationTime(request.getConsultationTime());
        } else {
            // Nếu xác nhận false, hủy lịch tư vấn
            schedule.setStatusConfirmed(false);
        }
        
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        // Lưu lại lịch tư vấn đã cập nhật
        consultationScheduleRepository.save(schedule);
    }
}
