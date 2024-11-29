package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.manage.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.UpdateConsultationScheduleRequest;

import java.time.LocalDate;

public interface IConsultationScheduleService {
    ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request, UserInformationEntity user);

    Page<ConsultationScheduleDTO> getConsultationScheduleByRole(UserInformationEntity user, String title,Boolean type, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConsultationScheduleRegistrationDTO registerForConsultation(ConsultationScheduleRegistrationRequest request, UserInformationEntity user);

    Page<ConsultationScheduleRegistrationDTO> getSchedulesJoinByUser(UserInformationEntity user, LocalDate startDate, LocalDate endDate, Pageable pageable);

    void cancelRegistrationForConsultation(Integer id, UserInformationEntity user);

    ManageConsultantScheduleDTO confirmConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);

    ConsultationScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId, Integer userId);

    ConsultationScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, boolean isAdmin, UpdateConsultationScheduleRequest request, String role, Integer userId);

    Page<ConsultationScheduleRegistrationMemberDTO> getMembersByConsultationSchedule(Integer consultationScheduleId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId);

    void deleteConsultationSchedule(Integer scheduleId, Integer departmentId, Integer userId, String role);

    ConsultationScheduleDTO getDetailConsultationScheduleByRole(Integer scheduleId, String role, Integer departmentId, Integer userId);
    public Page<ConsultationScheduleDTO> getConsultationScheduleForGuest(Pageable pageable);

}

