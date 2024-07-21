package studentConsulting.model.entity.news;

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
@Table(name = "news_shares")
@NoArgsConstructor
@AllArgsConstructor
public class NewsShareEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id", nullable = false)
    private Integer id; // Mã chia sẻ

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false, referencedColumnName = "id")
    private UserInformationEntity user; // Mã người dùng tham chiếu

    @ManyToOne
    @JoinColumn(name = "news_id", nullable = false, referencedColumnName = "id")
    private NewsEntity news; // Mã bài đăng tham chiếu

    @Column(name = "date_share", nullable = false, columnDefinition = "TIMESTAMP DEFAULT CURRENT_TIMESTAMP")
    private Timestamp dateShare; // Thời gian chia sẻ

    @Column(name = "share_status")
    private Boolean shareStatus; // Trạng thái chia sẻ (thành công hoặc thất bại)
}
