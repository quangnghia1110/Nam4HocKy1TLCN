package studentConsulting.model.entity.consultation;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.entity.authentication.UserInformationEntity;

import javax.persistence.*;
import java.sql.Timestamp;

@Data
@Builder
@Entity
@Table(name = "forwarded_infos")
@NoArgsConstructor
@AllArgsConstructor
public class ForwardedInfoEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã chuyển tiếp

    @ManyToOne
    @JoinColumn(name = "sender_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity sender; // Mã người gửi tham chiếu

    @ManyToOne
    @JoinColumn(name = "receiver_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity receiver; // Mã người nhận tham chiếu

    @Column(name = "info_type", nullable = false, length = 255)
    private String infoType; // Loại thông tin (file excel, lịch tư vấn)

    @Column(name = "file_path", length = 255)
    private String filePath; // Đường dẫn đến file nếu là file excel

    @Column(name = "created_at", nullable = false, updatable = false, insertable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp createdAt; // Thời gian tạo

    @Column(name = "updated_at", nullable = false, insertable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP")
    private Timestamp updatedAt; // Thời gian cập nhật
}

