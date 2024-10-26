package studentConsulting.service.interfaces.user;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.request.consultant.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;

import java.time.LocalDate;

public interface IUserConsultationScheduleService {
    ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request, UserInformationEntity user);

    Page<ConsultationScheduleDTO> getConsultationScheduleByRole(UserInformationEntity user, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ConsultationScheduleRegistrationDTO registerForConsultation(ConsultationScheduleRegistrationRequest request, UserInformationEntity user);

    Page<ConsultationScheduleRegistrationDTO> getSchedulesJoinByUser(UserInformationEntity user, LocalDate startDate, LocalDate endDate, Pageable pageable);

    void cancelRegistrationForConsultation(Integer id, UserInformationEntity user);

}

